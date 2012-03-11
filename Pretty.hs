{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pretty
    ( prettyExpr
    , prettyDecl
    , prettyProgram
    ) where

import Text.PrettyPrint

import AST

dot                = text "."

lam (Lam i e)      = text i <+> lam e
lam e              = dot <+> expr e

appR e@(Lam _ _)   = parens (expr e)
appR e             = expr e

app l@(Var _) r    = expr l <+> appR r
app (App l m) r    = app l m <+> appR r
app l         r    = parens (expr l) <+> appR r

expr (Var i)       = text i
expr e@(Lam _ _)   = text "\\" <> lam e
expr (App l r)     = app l r
expr (Let i e1 e2) = (text "let" <+> text i <+> equals <+> expr e1) $$
                     (text "in" <+> expr e2)
expr (Fix i e)     = text "fix" <+> text i <> dot <+> expr e

decl (i, e)        = text i <+> equals <+> expr e <> semi

program            = vcat . map decl . unProgram

prettyExpr    :: Expr -> Doc
prettyExpr    = expr
prettyDecl    :: (Id, Expr) -> Doc
prettyDecl    = decl
prettyProgram :: Program -> Doc
prettyProgram = program

instance Show Expr where
    show = render . expr

instance Show Program where
    show = render . program
