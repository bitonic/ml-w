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
    deriving (Eq, Show)

instance Error EvalError where
    noMsg = EvalError

class MonadError EvalError m => MonadEval m
instance MonadEval (ErrorT EvalError Identity)

-- | Performs beta reduction. See comment in 'stepExpr'.
step :: MonadEval m => Expr -> m (Maybe Expr)
step (Var _)       = return Nothing
step (Lam i e)     = (Lam i <$>) <$> step e
step (App e1 e2)   =
    do e2M <- step e2
       case e2M of
           Just e2' -> return $ Just (App e1 e2')
           Nothing  -> Just <$> apply e1 e2
step (Let i e1 e2) =
    do e1M <- step e1
       case e1M of
           Just e1' -> return $ Just (Let i e1' e2)
           Nothing  -> return $ Just (subst i e2 e1)
step (Fix i e)     = return $ Just (subst i e (Fix i e))

-- | Applies the second expression to the first.
apply :: MonadEval m => Expr -> Expr -> m Expr
apply (Lam i e1) e2  = return $ subst i e1 e2
apply e1         e2  = throwError $ NonFunctionApplication e1 e2

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
