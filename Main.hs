import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

import LazyZ.Syntax (lazyZparser, encodeLiteral)
import LazyZ.Encoding
import LazyZ.Program (compile, ExprP, bindBuiltins)
import LazyZ.Link (link, optimize)
import LazyZ.Expr (simpl, eval, apply, Expr(..), showUnlambda)
import LazyZ.LazyKSyntax (unlambdaParser)

import Control.Arrow
import Data.Char (chr, ord)
import qualified Data.Map as M
import Data.Either (lefts, rights)
import Control.Monad

build :: [(String, ExprP e)] -> Maybe (Expr e)
build = fmap (simpl . bindBuiltins . compile . optimize) . link "main" . M.fromList

run file = do
    Right prog <- parseFromFile unlambdaParser file
    input <- getContents
    putStr $ fst $ decode $ eval ((prog :: Expr ()) :$ encode input)
    where
        encode = fromList . map encodeNat . (++ repeat 256) . map ord
        decode = toList >>> map decodeNat >>> span (<256) >>> map chr *** head
        
main = getArgs >>= \args -> case args of
    ("build":xs) -> do
        progs <- mapM (parseFromFile lazyZparser) xs
        let errors = lefts $ progs
        case errors of
             [] -> case build $ concat $ rights $ progs of
                        Just x -> putStrLn $ showUnlambda $ void $ x >>= encodeLiteral
                        Nothing -> error "Linking Error: main is not defined"
             xs -> putStr $ unlines $ map show $ errors
    ("run":file:_) -> run file
    ("execute":xs) -> do
        progs <- mapM (parseFromFile lazyZparser) xs
        let errors = lefts $ progs
        case errors of
             [] -> case build $ concat $ rights $ progs of
                        Just x -> do
                            input <- getContents
                            putStr $ fst $ decode $ eval $ (x >>= encodeLiteral) :$ encode input
                        Nothing -> error "Linking Error: main is not defined"
             xs -> putStr $ unlines $ map show $ errors
    _ -> putStrLn "Usage: LazyZ (build|runlazyk) [input files]"
    where
        encode = fromList . map encodeNat . (++ repeat 256) . map ord
        decode = toList >>> map decodeNat >>> span (<256) >>> map chr *** head