{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parser
    ( parseExpr
    , parseExpr'
    , parseProgram
    , parseProgram'
    ) where             

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token hiding (parens)
import qualified Text.ParserCombinators.Parsec.Token as P

import AST
import Applicative

mlDef :: LanguageDef ()
mlDef = LanguageDef
        { commentStart    = "/*"
        , commentEnd      = "*/"
        , commentLine     = "//"
        , nestedComments  = True
        , identStart      = letter
        , identLetter     = alphaNum <|> char '_'
        , opStart         = mzero
        , opLetter        = mzero
        , reservedNames   = ["let", "fix", "in"]
        , reservedOpNames = ["\\", ".", "=", ";"]
        , caseSensitive   = True
        }

lexer     = P.makeTokenParser mlDef

lid       = P.identifier lexer

llet      = P.reserved lexer "let"
lfix      = P.reserved lexer "fix"
lin       = P.reserved lexer "in"

llam      = P.reservedOp lexer "\\"
ldot      = P.reservedOp lexer "."
lequal    = P.reservedOp lexer "="
lsemi     = P.reservedOp lexer ";"

parens    = P.parens lexer

var       = Var <$> lid
lam       = flip (foldr Lam) <$> (llam *> many1 lid) <*> (ldot *> expr)
let'      = Let <$> (llet *> lid) <*> (lequal *> expr) <*> (lin *> expr)
fix       = Fix <$> (lfix *> lid) <*> (ldot *> expr)
expr      = foldl App <$> p <*> many p
  where p = parens expr <|> lam <|> try let' <|> try fix <|> var
            <?> "expression"

decl      = (,) <$> lid <*> (lequal *> expr <* lsemi)
program   = Program <$> many decl

parseExpr     :: String -> Either ParseError Expr
parseExpr     = parse expr ""
parseExpr'    :: FilePath -> IO (Either ParseError Expr)
parseExpr' fn =  parse expr fn <$> readFile fn

parseProgram     :: String -> Either ParseError Program
parseProgram     = parse program ""
parseProgram'    :: FilePath -> IO (Either ParseError Program)
parseProgram' fn = parse program fn <$> readFile fn

readParse :: Parser a -> String -> [(a, String)]
readParse p = either (const []) (: []) . parse p' ""
    where p' = do x <- p
                  State {stateInput = input} <- getParserState
                  return (x, input)

instance Read Expr where
    readsPrec _ = readParse expr

instance Read Program where
    readsPrec _ = readParse program
