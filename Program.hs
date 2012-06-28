module LazyZ.Program where

import Prelude hiding (length)

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.List ((\\))
import qualified Data.Map as M

import Debug.Trace (traceShow)

import LazyZ.Expr hiding (length)
import qualified LazyZ.Combinator as LC

data ExprP e = Var String
             | Apply (ExprP e) (ExprP e)
             | Lambda String (ExprP e)
             | External e deriving (Show, Eq)

instance Functor ExprP where
    fmap f (External e) = External $ f e
    fmap _ (Var v) = Var v
    fmap f (Apply x y) = fmap f x `Apply` fmap f y
    fmap f (Lambda v b) = Lambda v $ fmap f b

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

unique :: ExprP e -> ExprP e
unique = unique' M.empty where
    unique' m (Apply x y) = unique' m x `Apply` unique' m y
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
    shorten' _ x = x

transformRecursion :: M.Map String (ExprP e) -> M.Map String (ExprP e)
transformRecursion table = M.mapWithKey trans table
    where
        trans name expr
            | name `notElem` vars expr = expr
            | otherwise = maybe expr (fix . Lambda name) $ linkBySubst name table
        fix f = replace "f" f $ decompile $ LC.fix (Free "f")

linkBySubst :: String -- entrypoint
    -> M.Map String (ExprP e) -- definitions
    -> Maybe (ExprP e)
linkBySubst point defs = link' (M.delete point defs) <$> M.lookup point defs
    where 
        link' defs e@(Var v) = maybe e id $ linkBySubst v defs
        link' defs (Apply f g) = link' defs f `Apply` link' defs g
        link' defs (Lambda v x) = Lambda v $ link' (M.delete v defs) x
        link' _ x = x

linkByLambda :: String -- entrypoint
    -> M.Map String (ExprP e) -- definitions
    -> Maybe (ExprP e)
linkByLambda point defs = chain <$> M.lookup point defs
    where
        chain def = foldr bind def (catMaybes $ children def)
        bind (n, def) x = Apply (Lambda n x) def 
        children = map parLink . vars
        parLink name = (,) name <$> flip linkByLambda defs name

builtins = [("I", I), ("K", K), ("S", S)]

bindBuiltins :: Expr e -> Expr e
bindBuiltins = flip (foldr $ uncurry subst) builtins

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

data DecoratedExprP e = Wrap (ExprP e) | CompileTime (DecoratedExprP e)
                        deriving (Show, Eq)

build :: String -> [(String, ExprP e)] -> Maybe (Expr e)
build point = fmap (bindBuiltins . compile . shorten)
    . linkByLambda point
    . transformRecursion
    . M.fromList