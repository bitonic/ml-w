{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Eval 
    ( EvalError (..)
    , stepExpr
    , evalExpr      
    , stepExpr'
    , evalExpr'
    , evalProgram
    , evalProgram'
    ) where

import Control.Monad.Error
import Control.Monad.Identity

import Applicative
import ML

-- | Errors that can arise when evaluating an expression. Right now just 1.
--
--   Note that if you typecheck before, these will never occur.
data EvalError
    = NonFunctionApplication Expr Expr
      -- ^ Trying to apply something to a non-function.
    | EvalError
      -- ^ Needed for the 'Error' instance.
    deriving Eq

instance Show EvalError where
    show (NonFunctionApplication e1 e2) =
        "EvalError: Trying to apply '" ++ show e2 ++ "' to non-function '" ++
        show e1 ++ "'."
    show EvalError                      = "Generic EvalError"

instance Error EvalError where
    noMsg = EvalError

class MonadError EvalError m => MonadEval m
instance MonadEval (ErrorT EvalError Identity)

-- | Performs beta reduction. See comment in 'stepExpr'.
step :: MonadEval m => Expr -> m (Maybe Expr)
step (Lam i e)     = (Lam i <$>) <$> step e
step (App e1 e2)   = apply e1 e2
-- We could desugar to an abstraction, we leave the let for clarity.
step (Let i e1 e2) =
    do e1M <- step e1
       return . Just $ case e1M of
           Just e1' -> Let i e1' e2
           Nothing  -> subst i e2 e1
step (Fix i e)     = return $ Just (subst i e (Fix i e))
step _             = return Nothing

-- | Tries to apply the second expression to the first.
apply :: MonadEval m => Expr -> Expr -> m (Maybe Expr)
apply (Lam i e1) e2 =
    do e2M <- step e2
       return . Just $ case e2M of
           Nothing  -> subst i e1 e2
           Just e2' -> App (Lam i e1) e2'
apply e1@(Var _) e2 = (App e1 <$>) <$> step e2
apply e1         e2 = (flip App e2 <$>) <$> step e1

-- | Performs substitution
subst :: Id
      -- ^ The variable we are substituting
      -> Expr
      -- ^ The expression we're substituting in
      -> Expr
      -- ^ The expression we're replacing the variable with.
      -> Expr
subst i e1@(Var i')    e2 | i == i'   = e2
                          | otherwise = e1
subst i e1@(Lam i' e2) e3 | i == i'   = e1
                          | otherwise = Lam i' (subst i e2 e3)
subst i (Let i' e1 e2) e3 | i == i'   = Let i' (subst i e1 e3) e2
                          | otherwise = Let i' (subst i e1 e3) (subst i e1 e3)
subst i e1@(Fix i' e2) e3 | i == i'   = e1
                          | otherwise = Fix i' (subst i e2 e3)
subst i (App e1 e2)    e3 = App (subst i e1 e3) (subst i e2 e3)

evaluate :: MonadEval m => Expr -> m Expr
evaluate e = maybe (return e) evaluate =<< step e

-- | Performs beta reduction. Might return an error. Returns 'Nothing' if the
--   expression is in normal form.
--
--   We evaluate to full normal form (in other words we evaluate inside lambdas)
--   in an eager manner (when applying we evaluate the arguments first).
--
--   This function does not check that all the variables are bound. It simply
--   performs substitution with applications and lets. In other words, ugly
--   thing can happen! Typecheck your expressions before.
stepExpr :: Expr -> Either EvalError (Maybe Expr)
stepExpr = runIdentity . runErrorT . step

-- | Steps the expression until it is in normal form.
evalExpr :: Expr -> Either EvalError Expr
evalExpr = runIdentity . runErrorT . evaluate

-- | Unsafe version of 'stepExpr'
stepExpr' :: Expr -> Maybe Expr
stepExpr' =
    either (\err -> error $ "Eval.evalExpr': " ++ show err) id . stepExpr

-- | Unsafe version of 'stepExpr'
evalExpr' :: Expr -> Expr
evalExpr' =
    either (\err -> error $ "Eval.evalExpr': " ++ show err) id . evalExpr

evalProgram :: Program -> Either EvalError Expr
evalProgram (Program decls' e'') =
    runIdentity . runErrorT . evaluate $ substs e'' decls'
  where
    substs = foldr (\(i, e') e -> subst i e e')

evalProgram' :: Program -> Expr
evalProgram' = 
    either (\err -> error $ "Eval.evalProgram': " ++ show err) id . evalProgram
