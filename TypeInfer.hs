{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- | This module implements the W algorithm for the small language we are using.
--
--   There is one minor annoyance: The 'Type' datatype distinguishes between
--   free type variables and quantified type variable, but the exposed functions
--   of this module should produce types completely free of free type
--   variables. This could be checked statically having a separate datatype
--   without free type variables, but I compromised for clarity and brevity.
module TypeInfer 
    ( -- * Data types
      TyVar
    , Type (..)
    , Scheme (..)
    , Assump (..)
      
      -- * Type inference
    , TypeError (..)
    , typeExpr
    , typeProgram
      
      -- * Pretty printing
    , prettyType
    , prettyScheme
    ) where

import Control.Monad.Error
import Control.Monad.State
import Data.List hiding (find)

import Text.PrettyPrint

import ML
import Applicative

-- | A type variable. We use integers for easy generation of new variables.
type TyVar = Int

-- | A data type to represent types. Note that the functions of this module
--   should return 'Type's without 'TyVar's (we want the schemes to have no free
--   variables).
data Type
    = TyVar TyVar
      -- ^ A type variable.
    | TyArr Type Type
      -- ^ The arrow type (A -> B)
    | TyGen Int
      -- ^ A quantified variable. We use a different constructor (separated from
      --   TyVar) so that there can be no clash between the two, and we
      --   immediately know what is what.
    deriving Eq

-- | A type scheme. The 'Int' represents the number of quantified variables
--   (must be >= 0).
--
--   Invariants: all the 'TyGen' in the scheme must be < of the 'Int'. If this
--   is not the case the program crashes badly (see 'freshen').
data Scheme = Scheme Int Type  
    deriving Eq

-- | A substitution, mapping 'TyVar's to 'Type's.
type Subst = [(TyVar, Type)]
    
nullSubst :: Subst
nullSubst = []

(+->)    :: TyVar -> Type -> Subst
tv +-> t = [(tv, t)]

-- | Type class with useful operations on types.
class Types t where
    -- | Applies the substitution to the given type.
    apply :: Subst -> t -> t
    -- | Gets all the free type variables in a given type.
    tvs   :: t -> [TyVar]

instance Types Type where
    apply s (TyVar tv)  = case lookup tv s of
                              Nothing -> TyVar tv
                              Just t  -> t
    apply s (TyArr l r) = TyArr (apply s l) (apply s r)
    -- We do not replace 'TyGen's (they're quantified).
    apply _ t           = t
    
    tvs (TyVar tv)      = [tv]
    tvs (TyArr t1 t2)   = tvs t1 `union` tvs t2
    tvs _               = []

-- | Useful instance on lists of types.
instance Types a => Types [a] where
    apply s = map (apply s)
    tvs     = nub . concat . map tvs

instance Types Scheme where
    apply s (Scheme i t) = Scheme i (apply s t)
    tvs (Scheme _ t)     = tvs t
    
infixr 4 @@
-- | Substitutions composition.
(@@)     :: Subst -> Subst -> Subst
s1 @@ s2 = [(tv, apply s1 t) | (tv, t) <- s2] ++ s1

-- | What can go wrong when inferring the types.
data TypeError
    = UnificationFail Type Type
      -- ^ Unification failed (e.g. when trying to unify a quantified variable
      --   with an arrow type).
    | InfiniteType TyVar Type
      -- ^ The user is trying to construct an infinite type, e.g. 'a = a -> b'.
    | UnboundVariable Id
      -- ^ Unbound variable (value, not type variable).
    | TypeError
      -- ^ Generic error, needed for the 'Error' instances.
    deriving (Show)

instance Error TypeError where
    noMsg = TypeError

-- | A 'Monad' we use for type inference. We obviously want to throw
--   'TypeError's (hence the 'MonadError') and we also want to generate fresh
--   'TyVar's - to do that we keep a state with an infinite list of integers,
--   initialized with @[1..]@.
class (MonadError TypeError m, MonadState [Int] m) => MonadInfer m

instance MonadInfer (ErrorT TypeError (State [Int]))

-- | Unifies two types, according to Robinson's algorithm.
unify :: MonadInfer m => Type -> Type -> m Subst
unify (TyArr t1 t2) (TyArr t1' t2') =
    do s1 <- unify t1 t1'
       s2 <- unify (apply s1 t2) (apply s1 t2')
       return (s2 @@ s1)
unify (TyVar tv)    t               = varBind tv t
unify t             (TyVar tv)      = varBind tv t
unify t1            t2              = throwError $ UnificationFail t1 t2

-- | Binds a given 'TyVar' to a type. Fails throwing an 'InfiniteType' error if
--   if the 'Type' already contains 'TyVar'.
varBind :: MonadInfer m => TyVar -> Type -> m Subst
varBind tv t | t == TyVar tv   = return nullSubst
             | tv `elem` tvs t = throwError $ InfiniteType tv t
             | otherwise       = return (tv +-> t)

-- | An assumption about the type of a (value) variable.
data Assump = Id :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: apply s sc
    tvs (_ :>: sc)     = tvs sc

-- | Gets a fresh type variable and wraps it in a type.
fresh :: MonadInfer m => m Type
fresh = TyVar <$> gets head <* modify tail

-- | Takes a 'Scheme' and replaces all the quantified type variables with fresh
--   'TyVar's, so that we can use the type when unifying.
--
--   Note: If the 'Scheme' is malformed this function will crash everything
--   using 'error'.
freshen :: MonadInfer m => Scheme -> m Type
freshen (Scheme gens t) =
    do sub <- zip [0..] <$> mapM (const fresh) [1..gens]
       return $ go sub t
  where
    go sub (TyGen i)     =
        case lookup i sub of
            Nothing -> error "Malformed Scheme"
            Just v  -> v
    go sub (TyArr t1 t2) = TyArr (go sub t1) (go sub t2)
    go _   t'            = t'

-- | Gets the 'Scheme' of a certain variable. Throws 'UnboundVariable' if the
--   variable is not present.
find :: MonadInfer m => [Assump] -> Id -> m Scheme
find []                 i  = throwError $ UnboundVariable i
find ((i :>: sc) : ctx) i' | i == i'   = return sc
                           | otherwise = find ctx i'

-- | Quantifies the provided type variables, returning a scheme.
quantify :: [TyVar] -> Type -> Scheme
quantify tvs' t = Scheme len (apply sub t)
  where
    len = length tvs'
    sub = map (\ix -> (tvs' !! ix, TyGen ix)) [0..len-1]

-- | The W algorithm itself: takes a list of 'Assump's, an 'Expr', and hopefully
--   returns a 'Scheme' representing the principal type of the 'Expr'.
--
--   This module exports 'typeExpr' and 'typeProgram', that run the 'MonadInfer'
--   returning an 'Either' for easier use.
typecheck :: MonadInfer m => [Assump] -> Expr -> m Scheme
typecheck sctx se = (\(_, t) -> quantify (tvs t) t) <$> go sctx se
  where
    go ctx (Var i)       = ((,) []) <$> (find ctx i >>= freshen)
    go ctx (Lam i e)     =
        do t1       <- fresh
           (s1, t2) <- go ((i :>: Scheme 0 t1) : ctx) e
           return (s1, apply s1 (TyArr t1 t2))
    go ctx (App t1 t2)   =
        do t3       <- fresh
           (s1, t4) <- go ctx t1
           (s2, t5) <- go (apply s1 ctx) t2
           s3       <- unify (apply s2 t4) (TyArr t5 t3)
           return (s3 @@ s2 @@ s1, apply s3 t3)
    go ctx (Let v e1 e2) =
        do (s1, t1) <- go ctx e1
           let ctx' = apply s1 ctx
               t2   = quantify (tvs t1 \\ tvs ctx') t1
           (s2, t3) <- go ((v :>: t2) : ctx') e2
           return (s2 @@ s1, t3)
    go ctx (Fix v e)     =
        do t1       <- fresh
           (s1, t2) <- go ((v :>: Scheme 0 t1) : ctx) e
           s2       <- unify (apply s1 t1) t2
           return (s2 @@ s1, apply s2 t2)

-- | Types an 'Expr' given a list of 'Assump'. Returns either a 'TypeError' if
--   the algorithm failed or a 'Scheme' if it succeeded.
typeExpr :: [Assump] -> Expr -> Either TypeError Scheme
typeExpr ctx e = evalState (runErrorT (typecheck ctx e)) [(1::Int)..]

-- | Types a list of declarations (a 'Program') returning the principal type for
--   each declaration and the type of the final expression.
typeProgram :: Program -> Either TypeError ([(Id, Scheme)], Scheme)
typeProgram (Program p' e') = evalState (runErrorT (go [] p')) [(1::Int)..]
  where
    go ctx []           =
        let ass = map (\(i :>: sc) -> (i, sc)) $ ctx
        in  ((,) ass) <$> typecheck ctx e'
    go ctx ((i, e) : p) =
        do put [1..]
           sc <- typecheck ctx e
           go (ctx ++ [(i :>: sc)]) p
 
-- Pretty printing

type' :: Type -> Doc
type' (TyGen i)               = int i
type' (TyVar i)               = text "v" <> int i
type' (TyArr t1@(TyGen _) t2) = type' t1 <+> text "->" <+> type' t2
type' (TyArr t1           t2) = parens (type' t1) <+> text "->" <+> type' t2

prettyType :: Type -> Doc
prettyType = type'

prettyScheme :: Scheme -> Doc
prettyScheme (Scheme _ t) = prettyType t

instance Show Type where
    show = render . type'

instance Show Scheme where
    show (Scheme _ t) = render . type' $ t
