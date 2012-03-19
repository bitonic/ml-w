{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|
This module defines some datatypes to represent a minimal ML-like language, plus
parsing and pretty-printing functions.

The syntax is:

@
program     ::= declaration ';' program | expression
declaration ::= id '=' expression
id          ::= [a-zA-Z][a-zA-Z0-9_]*
ids         ::= id+
expression  ::= id
             | '(' '\' ids . expression ')'
             | '(' expression expression ')'
             | '(' 'let' id '=' expression 'in' expression ')'
             | '(' 'fix' id . expression ')'
@

We'll omit parenthesis in the usual way - @a b c@ is equivalent to @(a b) c@.

Example:

@
s = \\ x y z . x z (y z);
k = \\ x y . x;
i = \\ x . x;
k i;
@
-}
module ML
    ( -- * Abstract syntax tree
      Id
    , Expr (..)
    , Decl
    , Program (..)
      
      -- * Parsing
    , parseExpr
    , parseExpr'
    , parseProgram
    , parseProgram'
      
      -- * Pretty printing
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

-- | An identifier (predictably a 'String').
type Id = String

-- | Data type representing lambda-calculus expressions.
data Expr
    = Var Id
      -- ^ A variable.
    | Lam Id Expr
      -- ^ A lambda abstraction.
    | App Expr Expr
      -- ^ An expression applied to another.
    | Let Id Expr Expr
      -- ^ Polymorphic let.
    | Fix Id Expr
      -- ^ Fixed point combinator (bye bye normalization).
    deriving Eq

-- | A declaration (binds a certain expression to a variable). We add this
--   abstraction on top of let so that we can write programs more easily
--   (leaving let for local declarations).
type Decl = (Id, Expr)

-- | A 'Program' is a list of declaration and an expression
--   representing what the program does. Each declaration can use
--   previous declarations only (no mutual recursion).
data Program = Program [Decl] Expr
    deriving Eq

-------------------------------------------------------------------------------
-- Lexing ---------------------------------------------------------------------
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Parsing --------------------------------------------------------------------
-------------------------------------------------------------------------------

pvar     = Var <$> lid
plam     = flip (foldr Lam) <$> (llam *> many1 lid) <*> (ldot *> pexpr)
plet     = Let <$> (llet *> lid) <*> (lequal *> pexpr) <*> (lin *> pexpr)
pfix     = Fix <$> (lfix *> lid) <*> (ldot *> pexpr)
pexpr    = plam <|> (foldl App <$> p <*> many p)
  where p = parens (plam <|> pexpr) <|> try plet <|> try pfix <|> pvar
            <?> "expression"

pdecl    = (,) <$> lid <*> (lequal *> pexpr <* lsemi)
pprogram = Program <$> many (try pdecl) <*> (pexpr <* lsemi)

parseExpr     :: String -> Either ParseError Expr
parseExpr     = parse (spaces *> pexpr <* eof) ""
parseExpr'    :: FilePath -> IO (Either ParseError Expr)
parseExpr' fn = parse (spaces *> pexpr <* eof) fn <$> readFile fn

parseProgram     :: String -> Either ParseError Program
parseProgram     = parse (spaces *> pprogram <* eof) ""
parseProgram'    :: FilePath -> IO (Either ParseError Program)
parseProgram' fn = parse (spaces *> pprogram <* eof) fn <$> readFile fn

readParse :: Parser a -> String -> [(a, String)]
readParse p = either (const []) (: []) . parse p' ""
    where p' = do x <- p
                  State {stateInput = input} <- getParserState
                  return (x, input)

instance Read Expr where
    readsPrec _ = readParse pexpr

instance Read Program where
    readsPrec _ = readParse pprogram

-------------------------------------------------------------------------------
-- Pretty printing ------------------------------------------------------------
-------------------------------------------------------------------------------

ppdot                    = text "."

pplam (Lam i e)          = text i <+> pplam e
pplam e                  = ppdot <+> ppexpr e

ppappR e@(Var _)         = ppexpr e
ppappR e                 = PP.parens (ppexpr e)

ppapp l@(Var _) r        = ppexpr l <+> ppappR r
ppapp (App l m) r        = ppapp l m <+> ppappR r
ppapp l         r        = PP.parens (ppexpr l) <+> ppappR r

ppexpr (Var i)           = text i
ppexpr e@(Lam _ _)       = text "\\" <> pplam e
ppexpr (App l r)         = ppapp l r
ppexpr (Let i e1 e2)     = (text "let" <+> text i <+> equals <+> ppexpr e1) $$
                           (text "in" <+> ppexpr e2)
ppexpr (Fix i e)         = text "fix" <+> text i <> ppdot <+> ppexpr e

ppdecl (i, e)            = text i <+> equals <+> ppexpr e <> PP.semi

ppprogram (Program es e) = vcat $ map ppdecl es ++ [ppexpr e <> PP.semi]

prettyExpr    :: Expr -> Doc
prettyExpr    = ppexpr
prettyDecl    :: (Id, Expr) -> Doc
prettyDecl    = ppdecl
prettyProgram :: Program -> Doc
prettyProgram = ppprogram

instance Show Expr where
    show = render . ppexpr

instance Show Program where
    show = render . ppprogram
