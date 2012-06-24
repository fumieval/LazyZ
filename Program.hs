module LazyZ.Program where
import Prelude hiding (length)
import LazyZ.Expr hiding (length)
import Data.List ((\\))
import qualified LazyZ.Combinator as LC

data ExprP e = Var String | Apply (ExprP e) (ExprP e) | Lambda String (ExprP e) | External e deriving Show

length :: ExprP e -> Int
length (Apply x y) = length x + length y + 1
length (Lambda v x) = length x + 1
length _ = 1

vars :: ExprP e -> [String]
vars (Var v) = [v]
vars (Apply x y) = vars x ++ vars y
vars (Lambda b x) = filter (/=b) $ vars x
vars _ = []

replace :: String -> ExprP e -> ExprP e -> ExprP e
replace v r (Var v') | v == v' = r
replace v r (Apply x y) = replace v r x `Apply` replace v r y
replace v r (Lambda v' x) | v /= v' = Lambda v' $ replace v r x
replace _ _ x = x

compile :: ExprP e -> Expr e
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

builtins = [("I", I), ("K", K), ("S", S)]

bindBuiltins :: Expr e -> Expr e
bindBuiltins = flip (foldr $ uncurry subst) builtins

