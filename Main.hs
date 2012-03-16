module Main (main) where

import Text.PrettyPrint

import ML
import TypeInfer

showTypes :: Program -> ([(Id, Scheme)], Scheme) -> String
showTypes (Program decls' e') (ts', t') =
    render (ppDecls decls' ts' $$ ppE e' t')
  where
    ppE e t = prettyExpr e <+> colon <+> prettyScheme t
    
    ppDecls [] [] = text ""
    ppDecls ((i, e) : decls) ((_, t) : ts) =
        text ""                               $$
        (text i <+> colon <+> prettyScheme t) $$
        (text i <+> equals <+> prettyExpr e)  $$
        ppDecls decls ts
    ppDecls _ _ = error "ppDecls: lists of different length"

{-|
Reads a 'Program' file from standard input, parses it and typechecks it.

Example:

@
cat tests\/ski.ml | ./ML
@
-}
main :: IO ()
main = do
    s <- getContents
    case parseProgram s of
        Left err -> error $ "Parse error: " ++ show err
        Right p  -> case typeProgram p of
            Left err -> error $ "Type error: " ++ show err
            Right ts -> putStrLn $ showTypes p ts
