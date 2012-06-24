{-# LANGUAGE Rank2Types #-}
module LazyZ.Link where

import Control.Applicative
import Data.List as L
import LazyZ.Program as P
import LazyZ.Expr (Expr(..))
import Data.Maybe
import qualified Data.Map as M

-- unique :: M.Map String (ExprP e) -> M.Map String (ExprP e)

optimize :: ExprP e -> ExprP e
optimize = optimize' True
    where
        -- optimize' _ e@(Apply (Lambda v x) y)
        --    | P.length x' < P.length e = x'
        --    where
        --        x'= replace v y x
        optimize' _ (Lambda v x) = Lambda v $ optimize x
        optimize' True (Apply x y) = optimize' False $ optimize x `Apply` optimize y
        optimize' _ x = x


transformRecursion :: M.Map String (ExprP e) -> M.Map String (ExprP e)
transformRecursion table = M.mapWithKey trans table
    where
        trans name expr
            | name `elem` vars expr = fix $ Lambda name $ fromJust $ link name table
            | otherwise = expr
        fix f = replace "f" f $ decompile $ S :$ I :$ I :$ (S :$ (K :$ Free "f") :$ (S :$ I :$ I))

link :: String -- entrypoint
    -> M.Map String (ExprP e) -- definitions
    -> Maybe (ExprP e)
link entrypoint defs = link' (M.delete entrypoint defs) <$> M.lookup entrypoint defs
    where
        link' defs' e@(Var v) = maybe e id $ link v defs'
        link' defs' (Apply f g) = link' defs' f `Apply` link' defs' g
        link' defs' (Lambda v x) = Lambda v $ link' (M.delete v defs') x
        link' _ x = x

toMap :: [(String, ExprP e)] -> M.Map String (ExprP e)
toMap = M.fromList