{-# LANGUAGE DeriveFunctor #-}
module Language.LazyZ.Program (
    ExprP(..)
    , length
    , vars
    , replace
    , linkAll
    ) where

import Prelude hiding (length)
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Combinator.Expr as Expr
import Control.Monad.State
import Data.Maybe (catMaybes)

data ExprP e = Var String
             | Apply (ExprP e) (ExprP e)
             | Lambda String (ExprP e)
			 | LiftExpr (Expr.Expr e)
             | External e deriving (Show, Eq, Functor)

length :: ExprP e -> Int
length (Apply x y) = length x + length y + 1
length (Lambda v x) = length x + 1
length (LiftExpr e) = Expr.length e

length _ = 1

vars :: ExprP e -> [String]
vars (Var v) = [v]
vars (Apply x y) = vars x ++ vars y
vars (Lambda b x) = filter (/=b) $ vars x
vars (LiftExpr e) = Expr.frees e
vars _ = []

replace :: String -> ExprP e -> ExprP e -> ExprP e
replace v r (Var v') | v == v' = r
replace v r (Apply x y) = replace v r x `Apply` replace v r y
replace v r (Lambda v' x) | v /= v' = Lambda v' $ replace v r x
replace _ _ x = x

unique :: ExprP e -> ExprP e
unique expr = uniq expr `evalState` 0 where
    uniq :: ExprP e -> State Int (ExprP e)
    uniq (Apply x y) = Apply <$> uniq x <*> uniq y 
    uniq (Lambda v x) = do
        n <- get
        put $ succ n
        x' <- uniq $ replace v (Var (show n)) x
        return $ Lambda (show n) x'
    uniq x = return x

shorten :: ExprP e -> ExprP e
shorten = fuse True . unique where
    fuse _ e@(Apply (Lambda v x) y)
         | fromIntegral (length e') <= fromIntegral (length e) + 3 = e'
        where e' = fuse True (replace v y x)
    fuse True (Apply f g) = fuse False $ fuse True f `Apply` fuse True g
    fuse _ (Lambda v x) = Lambda v $ fuse True x
    fuse _ x = x

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
        chain def = foldr bind def (catMaybes $ map parLink $ vars def)
        bind (n, def) x = Apply (Lambda n x) def
        parLink name = (,) name <$> linkByLambda name defs

transformRecursion :: M.Map String (ExprP e) -> M.Map String (ExprP e)
transformRecursion table = M.mapWithKey trans table where
    trans name expr
        | name `notElem` vars expr = expr
        | otherwise = maybe expr (Apply (Var "prim_fix") . Lambda name) $ linkBySubst name table

linkAll :: String -> [(String, ExprP e)] -> Maybe (ExprP e)
linkAll point = fmap shorten
    . linkByLambda point
    . transformRecursion
    . M.fromList 
