{-# LANGUAGE PackageImports #-}
import "LazyZ" Language.LazyZ.Builder
import "LazyZ" Language.LazyK.Syntax
import System.Environment

main = do
    args <- getArgs
    case args of
        ("build":files) -> do
            result <- buildFiles files
            putStrLn $ showUnlambda result