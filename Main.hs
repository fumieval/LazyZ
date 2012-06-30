module Main where

import Control.Monad
import Data.Either
import System.IO
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

import LazyZ.Syntax (lazyZparser, encodeLiteral)
import LazyZ.Expr (Expr)
import LazyZ.Builder(build)
import LazyZ.LazyK
import LazyZ.Interface (runLazyZWithSocket)

buildFiles :: [FilePath] -> IO (Either String (Expr ()))
buildFiles files = (`fmap`mapM (parseFromFile lazyZparser) files) $ \progs ->
    case lefts progs of
         [] -> case build "main" $ concat $ rights $ progs of
                    Just expr -> Right $ void $ expr >>= encodeLiteral
                    Nothing   -> Left "Linking Error\n"
         errors -> Left $ unlines $ map show $ errors

main = getArgs >>= \args -> case args of
    ("build":xs) -> buildFiles xs
        >>= either (hPutStr stderr) (putStrLn . showUnlambda)
    
    ("execute":xs) -> buildFiles xs
        >>= either (hPutStr stderr) runAndPrint
    
    ("run":file:_) -> parseFromFile unlambdaParser file
        >>= either (hPutStr stderr . show) runAndPrint
		
    ("runZ":file:_) -> parseFromFile unlambdaParser file
        >>= either (hPutStr stderr . show) runLazyZWithSocket
		
    ("executeZ":xs) -> buildFiles xs
        >>= either (hPutStr stderr) runLazyZWithSocket
    
    _ -> putStrLn "Usage: LazyZ (build|execute|run|executeZ|runZ) [input files]"
    where
        runAndPrint :: Expr () -> IO ()
        runAndPrint = flip fmap getContents . runLazyK >=> putStr
