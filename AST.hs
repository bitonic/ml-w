module AST
    ( Id
    , Expr (..)
    , Program
    ) where

type Id = String

data Expr
    = Var Id
    | Lam Id Expr
    | App Expr Expr
    | Let Id Expr Expr
    | Fix Id Expr
    deriving (Eq, Show)

type Program = [Expr]