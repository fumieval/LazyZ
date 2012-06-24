
import LazyZ.Syntax (lazyZparser, encodeLiteral)
import LazyZ.Program (build)
import LazyZ.Expr (simpl, Expr, showUnlambda)
import LazyZ.LazyK (unlambdaParser, runLazyK)

import Text.Parsec.Prim (parse)
import Text.Parsec.String (parseFromFile)
import Control.Applicative

import Control.Monad
import Data.Either
import System.IO
import System.Environment (getArgs)

buildFiles :: [FilePath] -> IO (Either String (Expr ()))
buildFiles files = mapM (parseFromFile lazyZparser) files >>= \progs ->
    case lefts $ progs of
         [] -> case build "main" $ concat $ rights $ progs of
                    Just expr -> return $ Right $ void $ expr >>= encodeLiteral
                    Nothing -> return $ Left "Linking Error: main is not defined\n"
         errors -> return $ Left $ unlines $ map show $ errors

main = getArgs >>= \args -> case args of
    ("build":xs) -> buildFiles xs
        >>= either (hPutStr stderr) (putStrLn . showUnlambda)
    
    ("execute":xs) -> buildFiles xs
        >>= either (hPutStr stderr) runAndPrint
    
    ("run":file:_) -> parseFromFile unlambdaParser file
        >>= either (hPutStr stderr . show) runAndPrint
    ("run":_) -> parse unlambdaParser "stdin" <$> getLine
        >>= either (hPutStr stderr . show) runAndPrint
    
    _ -> putStrLn "Usage: LazyZ (build|execute|run) [input files]"
    where
        runAndPrint :: Expr () -> IO ()
        runAndPrint = flip fmap getContents . runLazyK >=> putStr
