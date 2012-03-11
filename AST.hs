module AST
    ( Id
    , Expr (..)
    , Decl
    , Program (..)
    ) where

type Id = String

data Expr
    = Var Id
    | Lam Id Expr
    | App Expr Expr
    | Let Id Expr Expr
    | Fix Id Expr
    deriving Eq

type Decl = (Id, Expr)

newtype Program = Program {unProgram :: [Decl]}