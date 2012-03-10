{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parser
    ( parseExpr
    , parseExpr'
    , parseProgram
    , parseProgram'
    ) where             

import Control.Applicative
import Control.Monad
import Control.Monad.Identity hiding (fix)
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.ParserCombinators.Parsec.Token (GenLanguageDef (..))
import qualified Text.ParserCombinators.Parsec.Token as P

import AST

mlDef :: GenLanguageDef String () Identity
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

lexer   = P.makeTokenParser mlDef

lid     = P.identifier lexer

llet    = P.reserved lexer "let"
lfix    = P.reserved lexer "fix"
lin     = P.reserved lexer "in"

llam    = P.reservedOp lexer "\\"
ldot    = P.reservedOp lexer "."
lequal  = P.reservedOp lexer "="
lsemi   = P.reservedOp lexer ";"

parens  = P.parens lexer

var     = Var <$> lid
lam     = flip (foldr Lam) <$> (llam *> many1 lid) <*> (ldot *> expr)
let'    = Let <$> (llet *> lid) <*> (lequal *> expr) <*> (lin *> expr)
fix     = Fix <$> (lfix *> lid) <*> (ldot *> expr)
expr    = parens expr <|> lam <|> try let' <|> try fix <|> var <?> "expression"
appExpr = foldl App <$> expr <*> many expr

program = sepBy1 appExpr lsemi <* lsemi

parseExpr       :: FilePath -> IO (Either ParseError Expr)
parseExpr fname =  parse appExpr fname <$> readFile fname
parseExpr'      :: String -> Either ParseError Expr
parseExpr'      = parse appExpr ""

parseProgram       :: FilePath -> IO (Either ParseError Program)
parseProgram fname = parse program fname <$> readFile fname
parseProgram'      :: String -> Either ParseError Program
parseProgram'      = parse program ""

instance Read Expr where
    readsPrec _ = either (const []) (: []) . parse p ""
      where
        p = do
            e <- appExpr
            State {stateInput = input} <- getParserState
            return (e, input)
