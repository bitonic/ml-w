{-# LANGUAGE FlexibleContexts, FlexibleInstances, TupleSections #-}
module TypeInfer 
    ( TyVar
    , Type (TyGen, TyArr)
    , Scheme (..)
    , typeExpr
    , typeProgram
    ) where

import Control.Monad.Error
import Control.Monad.State
import Data.List hiding (find)

import Text.PrettyPrint

import AST
import Applicative

type TyVar = Int

data Type
    = TyVar TyVar
    | TyArr Type Type
    | TyGen Int
    deriving Eq

data Scheme = Scheme Int Type
    deriving Eq

type Subst = [(TyVar, Type)]
    
nullSubst :: Subst
nullSubst = []

(+->)   :: TyVar -> Type -> Subst
u +-> t = [(u, t)]

class Types t where
    apply :: Subst -> t -> t
    tv    :: t -> [TyVar]

instance Types Type where
    apply s (TyVar u)   = case lookup u s of
                              Nothing -> TyVar u
                              Just t  -> t
    apply s (TyArr l r) = TyArr (apply s l) (apply s r)
    apply _ t           = t
    
    tv (TyVar u)        = [u]
    tv (TyArr l r)      = tv l `union` tv r
    tv _                = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv      = nub . concat . map tv

instance Types Scheme where
    apply s (Scheme i t) = Scheme i (apply s t)
    tv (Scheme _ t)      = tv t
    
infixr 4 @@
(@@)     :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

data TypeError
    = UnificationFail Type Type
    | InfiniteType TyVar Type
    | UnboundVariable String
    | TypeError
    deriving (Show)

instance Error TypeError where
    noMsg = TypeError

unify :: MonadError TypeError m => Type -> Type -> m Subst
unify (TyArr l r) (TyArr l' r') = do s1 <- unify l l'
                                     s2 <- unify (apply s1 r) (apply s1 r')
                                     return (s2 @@ s1)
unify (TyVar u)   t             = varBind u t
unify t           (TyVar u)     = varBind u t
unify t1          t2            = throwError $ UnificationFail t1 t2

varBind :: MonadError TypeError m => TyVar -> Type -> m Subst
varBind u t | t == TyVar u  = return nullSubst
            | u `elem` tv t = throwError $ InfiniteType u t
            | otherwise     = return (u +-> t)

data Assump = Id :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: apply s sc
    tv (_ :>: sc)      = tv sc

class (MonadError TypeError m, MonadState [Int] m) => MonadInfer m

instance MonadInfer (ErrorT TypeError (State [Int]))

fresh :: MonadInfer m => m Type
fresh = TyVar <$> gets head <* modify tail

freshen :: MonadInfer m => Scheme -> m Type
freshen (Scheme gens t) =
    do sub <- zip [1..gens] <$> mapM (const fresh) [1..gens]
       return $ go sub t
  where
    go sub (TyGen i) =
        case lookup i sub of
            Nothing -> error "Malformed Scheme"
            Just v  -> v
    go sub (TyArr l r) = TyArr (go sub l) (go sub r)
    go _   t'          = t'

find :: MonadInfer m => [Assump] -> Id -> m Scheme
find []                 i  = throwError $ UnboundVariable i
find ((i :>: sc) : ctx) i' | i == i'   = return sc
                           | otherwise = find ctx i'

quantify :: [TyVar] -> Type -> Scheme
quantify tvs t = Scheme len (apply sub t)
  where
    len = length tvs
    sub = map (\ix -> (tvs !! ix, TyGen ix)) [0..len-1]
    
typecheck :: MonadInfer m => [Assump] -> Expr -> m Scheme
typecheck sctx se = (\(_, t) -> quantify (tv t) t) <$> go sctx se
  where
    go ctx (Var i)       = ([] ,) <$> (find ctx i >>= freshen)
    go ctx (Lam i e)     =
        do t        <- fresh
           (s1, a)  <- go ((i :>: Scheme 0 t) : ctx) e
           return (s1, apply s1 (TyArr t a))
    go ctx (App l r)     =
        do t        <- fresh
           (s1, a)  <- go ctx l
           (s2, b)  <- go (apply s1 ctx) r
           s3       <- unify (apply s2 a) (TyArr b t)
           return (s3 @@ s2 @@ s1, apply s3 t)
    go ctx (Let v e1 e2) =
        do (s1, a)  <- go ctx e1
           let ctx' = apply s1 ctx
               a'   = quantify (tv a \\ tv ctx') a
           (s2, b)  <- go ((v :>: a') : ctx') e2
           return (s2 @@ s1, b)
    go ctx (Fix v e)     =
        do t        <- fresh
           (s1, a)  <- go ((v :>: Scheme 0 t) : ctx) e
           s2       <- unify (apply s1 t) a
           return (s2 @@ s1, apply s2 a)

typeExpr :: [Assump] -> Expr -> Either TypeError Scheme
typeExpr ctx e = evalState (runErrorT (typecheck ctx e)) [(1::Int)..]

typeProgram :: Program -> Either TypeError ([(Id, Scheme)], Scheme)
typeProgram (Program p' e') = evalState (runErrorT (go [] p')) [(1 :: Int)..]
  where
    go ctx []           =
        let ass = map (\(i :>: sc) -> (i, sc)) $ ctx
        in  (ass,) <$> typecheck ctx e'
    go ctx ((i, e) : p) =
        do put [1..]
           sc <- typecheck ctx e
           go (ctx ++ [(i :>: sc)]) p

-- Pretty printing

type' :: Type -> Doc
type' (TyGen i)             = int i
type' (TyVar i)             = text "v" <> int i
type' (TyArr t@(TyGen _) r) = type' t <+> text "->" <+> type' r
type' (TyArr l           r) = parens (type' l) <+> text "->" <+> type' r

instance Show Type where
    show = render . type'
