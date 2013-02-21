{-# LANGUAGE ImplicitParams #-}
module Language.LazyZ.Builder where
import Data.Combinator.Expr
import Language.LazyZ.Program
import Language.LazyZ.Syntax
import Text.Parsec.String
import Data.Void
import qualified Data.Map as M
import Debug.Trace

compile :: Eq e => ExprP e -> Expr e
compile (Apply f x) = compile f :$ compile x
compile (Lambda p e) = bindee p $ compile e
compile (Var i) = Free i
compile (LiftExpr e) = e
compile (External x) = Extern x

build :: Eq e => String -> [(String, ExprP e)] -> Maybe (Expr e)
build point = fmap (compile) . linkAll point

buildFiles :: [FilePath] -> IO (Expr Void)
buildFiles files = do
    r <- fmap sequence $ mapM (parseFromFile parseOpTable) files
    ops <- case r of
        Left e -> fail $ show e
        Right a -> return a
    
    mapM (parseFromFile $ parseDecs $ concat ops) files >>= \r -> case sequence r of
        Left err -> error $ show err
        Right progs -> case build "main" $ concat progs of
            Just expr -> return expr
            Nothing   -> fail "Linking error"