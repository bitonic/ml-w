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


-- parsec 2 doeesn't have an Applicative instance...

(<$>)  :: Functor f => (a -> b) -> f a -> f b
(<$>)  = fmap

(<*>)  :: Monad m => m (a -> b) -> m a -> m b
(<*>)  = ap

(*>)   :: Monad m => m a -> m b -> m b
(*>)   = (>>)

(<*)   :: Monad m => m a -> m b -> m a
m <* n =  do x <- m; n; return x
