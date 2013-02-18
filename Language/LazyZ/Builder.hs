{-# LANGUAGE ImplicitParams #-}
module Language.LazyZ.Builder where
import Data.Combinator.Expr
import Language.LazyZ.Program
import Language.LazyZ.Syntax
import Text.Parsec.String

import qualified Data.Map as M

compile :: Eq e => ExprP e -> Expr e
compile (Var "I") = I
compile (Var "K") = K
compile (Var "S") = S
compile (Apply f x) = compile f :$ compile x
compile (Lambda p e) = bindee p $ compile e
compile (Var i) = Free i
compile (Embed e) = e
compile (External x) = Extern x

build :: Eq e => String -> [(String, ExprP e)] -> Maybe (Expr e)
build point  = fmap compile . linkAll point

buildFiles :: [FilePath] -> IO (Expr Literal)
buildFiles files = do
    r <- fmap sequence $ mapM (parseFromFile parseOpTable) files
    ops <- case r of
        Left e -> fail $ show e
        Right a -> return a
    let ?optable = concat ops
    
    mapM (parseFromFile parseDecs) files >>= \r -> case sequence r of
        Left err -> error $ show err
        Right progs -> case build "main" $ concat progs of
            Just expr -> return expr
            Nothing   -> fail "Linking error"