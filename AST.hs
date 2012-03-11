{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module AST
    ( Id
    , Expr (..)
    , Decl
    , Program (..)
      
    , parseExpr
    , parseExpr'
    , parseProgram
    , parseProgram'
      
    , prettyExpr
    , prettyDecl
    , prettyProgram
    ) where

import Control.Monad

import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Token hiding (parens, dot)

import Text.PrettyPrint hiding (parens)
import qualified Text.PrettyPrint as PP

import Applicative

type Id = String

data Expr
    = Var Id
    | Lam Id Expr
    | App Expr Expr
    | Let Id Expr Expr
    | Fix Id Expr
    deriving Eq

type Decl = (Id, Expr)

data Program = Program [Decl] Expr
    deriving Eq

-- Parsing

mlDef :: LanguageDef ()
mlDef = LanguageDef
        { commentStart    = "/*"
        , commentEnd      = "*/"
        , commentLine     = "//"
        , nestedComments  = True
        , identStart      = letter
        , identLetter     = alphaNum <|> P.char '_'
        , opStart         = mzero
        , opLetter        = mzero
        , reservedNames   = ["let", "fix", "in"]
        , reservedOpNames = ["\\", "="]
        , caseSensitive   = True
        }

lexer   = P.makeTokenParser mlDef

lid     = P.identifier lexer

llet    = P.reserved lexer "let"
lfix    = P.reserved lexer "fix"
lin     = P.reserved lexer "in"

llam    = P.reservedOp lexer "\\"
ldot    = P.dot lexer
lequal  = P.reservedOp lexer "="
lsemi   = P.semi lexer

parens  = P.parens lexer

var     = Var <$> lid
lam     = flip (foldr Lam) <$> (llam *> many1 lid) <*> (ldot *> expr)
let'    = Let <$> (llet *> lid) <*> (lequal *> expr) <*> (lin *> expr)
fix     = Fix <$> (lfix *> lid) <*> (ldot *> expr)
expr    = lam <|> (foldl App <$> p <*> many p)
  where p = parens (lam <|> expr) <|> try let' <|> try fix <|> var
            <?> "expression"

decl    = (,) <$> lid <*> (lequal *> expr <* lsemi)
program = Program <$> many (try decl) <*> (expr <* lsemi)

parseExpr     :: String -> Either ParseError Expr
parseExpr     = parse (spaces *> expr <* eof) ""
parseExpr'    :: FilePath -> IO (Either ParseError Expr)
parseExpr' fn = parse (spaces *> expr <* eof) fn <$> readFile fn

parseProgram     :: String -> Either ParseError Program
parseProgram     = parse (spaces *> program <* eof) ""
parseProgram'    :: FilePath -> IO (Either ParseError Program)
parseProgram' fn = parse (spaces *> program <* eof) fn <$> readFile fn

readParse :: Parser a -> String -> [(a, String)]
readParse p = either (const []) (: []) . parse p' ""
    where p' = do x <- p
                  State {stateInput = input} <- getParserState
                  return (x, input)

instance Read Expr where
    readsPrec _ = readParse expr

instance Read Program where
    readsPrec _ = readParse program

-- Pretty printing

pdot                    = text "."

plam (Lam i e)          = text i <+> plam e
plam e                  = pdot <+> pexpr e

pappR e@(Var _)         = pexpr e
pappR e                 = PP.parens (pexpr e)

papp l@(Var _) r        = pexpr l <+> pappR r
papp (App l m) r        = papp l m <+> pappR r
papp l         r        = PP.parens (pexpr l) <+> pappR r

pexpr (Var i)           = text i
pexpr e@(Lam _ _)       = text "\\" <> plam e
pexpr (App l r)         = papp l r
pexpr (Let i e1 e2)     = (text "let" <+> text i <+> equals <+> pexpr e1) $$
                          (text "in" <+> pexpr e2)
pexpr (Fix i e)         = text "fix" <+> text i <> pdot <+> pexpr e

pdecl (i, e)            = text i <+> equals <+> pexpr e <> PP.semi

pprogram (Program es e) = vcat $ map pdecl es ++ [pexpr e <> PP.semi]

prettyExpr    :: Expr -> Doc
prettyExpr    = pexpr
prettyDecl    :: (Id, Expr) -> Doc
prettyDecl    = pdecl
prettyProgram :: Program -> Doc
prettyProgram = pprogram

instance Show Expr where
    show = render . pexpr

instance Show Program where
    show = render . pprogram
