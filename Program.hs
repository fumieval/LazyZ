module LazyZ.Program where

import Prelude hiding (length)

import Control.Arrow (second)
import Control.Applicative
import Data.List ((\\))
import qualified Data.Map as M

import LazyZ.Expr hiding (length)
import qualified LazyZ.Combinator as LC

data ExprP e = Var String
             | Apply (ExprP e) (ExprP e)
             | Lambda String (ExprP e)
             | External e
             | Halt (ExprP e) deriving (Show, Eq)

type MustStop = Bool

length :: ExprP e -> Int
length (Apply x y) = length x + length y + 1
length (Lambda v x) = length x + 1
length (Halt x) = length x
length _ = 1

vars :: ExprP e -> [String]
vars (Var v) = [v]
vars (Apply x y) = vars x ++ vars y
vars (Lambda b x) = filter (/=b) $ vars x
vars (Halt x) = vars x
vars _ = []

replace :: String -> ExprP e -> ExprP e -> ExprP e
replace v r (Var v') | v == v' = r
replace v r (Apply x y) = replace v r x `Apply` replace v r y
replace v r (Lambda v' x) | v /= v' = Lambda v' $ replace v r x
replace v r (Halt x) = Halt $ replace v r x
replace _ _ x = x

unique :: ExprP e -> ExprP e
unique = unique' M.empty where
    unique' m (Apply x y) = unique' m x `Apply` unique' m y
    unique' m (Halt x) = Halt $ unique' m x
    unique' m (Lambda v x)
        | n == 0 = Lambda v $ unique' m' x
        | otherwise = Lambda v' $ unique' m' (replace v (Var v') x)
        where
            n = maybe 0 id $ M.lookup v m
            v' = replicate n '\'' ++ v
            m' = (M.insertWith (+) v 1 m)
    unique' _ x = x

shorten :: ExprP e -> ExprP e
shorten = shorten' True . unique where
    shorten' _ e@(Apply (Lambda v x) y)
        | length x' < length e = shorten' True x'
        where
            x' = replace v y x
    
    shorten' True (Apply x y) = shorten' False
        $ shorten' True x `Apply` shorten' True y
    shorten' _ (Lambda v x) = Lambda v $ shorten' True x
    shorten' _ (Halt x) = Halt $ shorten' True x
    shorten' _ x = x

transformRecursion :: M.Map String (ExprP e) -> M.Map String (ExprP e)
transformRecursion table = M.mapWithKey trans table
    where
        trans name expr
            | name `notElem` vars expr = expr
            | otherwise = maybe expr (fix . Lambda name) $ link name table
        fix f = replace "f" f $ decompile $ LC.fix (Free "f")

link :: String -- entrypoint
    -> M.Map String (ExprP e) -- definitions
    -> Maybe (ExprP e)
link point defs = link' (M.delete point defs) <$> M.lookup point defs
    where 
        link' defs e@(Var v) = maybe e id $ link v defs
        link' defs (Apply f g) = link' defs f `Apply` link' defs g
        link' defs (Halt x) = Halt $ link' defs x
        link' defs (Lambda v x) = Lambda v $ link' (M.delete v defs) x
        link' _ x = x

builtins = [("I", I), ("K", K), ("S", S)]

bindBuiltins :: Expr e -> Expr e
bindBuiltins = flip (foldr $ uncurry subst) builtins

compile :: ExprP e -> Expr e
compile (Apply f x) = compile f :$ compile x
compile (Lambda p e) = bindee p $ compile e
compile (Var i) = Free i
compile (External x) = Extern x
compile (Halt x) = compile x

decompile :: Expr e -> ExprP e
decompile (x :$ y) = decompile x `Apply` decompile y
decompile (Free v) = Var v
decompile (Extern x) = External x
decompile I = Var "I"
decompile K = Var "K"
decompile S = Var "S"

build :: String -> [(String, ExprP e)] -> Maybe (Expr e)
build point = fmap (simpl . bindBuiltins . compile . shorten)
    . link point
    . transformRecursion
    . M.fromList