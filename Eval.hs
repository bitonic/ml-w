{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Eval 
    ( EvalError (..)
    , evalExpr
    , stepExpr
    ) where

import Control.Monad.Error
import Control.Monad.Identity

import Applicative
import ML

data EvalError
    = NonFunctionApplication Expr Expr
    | EvalError
    deriving (Eq, Show)

instance Error EvalError where
    noMsg = EvalError

class MonadError EvalError m => MonadEval m
instance MonadEval (ErrorT EvalError Identity)

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

apply :: MonadEval m => Expr -> Expr -> m Expr
apply (Lam i e1) e2  = return $ subst i e1 e2
apply e1         e2  = throwError $ NonFunctionApplication e1 e2

subst :: Id -> Expr -> Expr -> Expr
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

stepExpr :: Expr -> Either EvalError (Maybe Expr)
stepExpr = runIdentity . runErrorT . step

evalExpr :: Expr -> Either EvalError Expr
evalExpr = runIdentity . runErrorT . evaluate
