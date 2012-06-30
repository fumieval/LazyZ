module LazyZ.Builder where
import LazyZ.Expr
import LazyZ.Program
import qualified LazyZ.Combinator as LC

import qualified Data.Map as M

builtins = [("I", I), ("K", K), ("S", S)]

bindBuiltins :: Expr e -> Expr e
bindBuiltins = flip (foldr $ uncurry subst) builtins

compile :: Eq e => ExprP e -> Expr e
compile (Apply f x) = compile f :$ compile x
compile (Lambda p e) = bindee p $ compile e
compile (Var i) = Free i
compile (External x) = Extern x

decompile :: Expr e -> ExprP e
decompile (x :$ y) = decompile x `Apply` decompile y
decompile (Free v) = Var v
decompile (Extern x) = External x
decompile I = Var "I"
decompile K = Var "K"
decompile S = Var "S" 

build :: Eq e => String -> [(String, ExprP e)] -> Maybe (Expr e)
build point = fmap (bindBuiltins . compile . shorten)
    . linkByLambda point
    . transformRecursion
    . M.fromList 
