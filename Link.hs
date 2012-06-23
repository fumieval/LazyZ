{-# LANGUAGE Rank2Types #-}
module LazyZ.Link where

import Control.Applicative
import Data.List as L
import LazyZ.Program as P

optimize :: ExprP e -> ExprP e
optimize = optimize' True
    where
        optimize' _ (Apply (Lambda v e) x)
            | n == 1  = optimize $ replace v x e
            | otherwise = Apply (Lambda v (optimize e)) x
            where
                n = L.length $ filter (==v) $ vars e
        optimize' _ (Lambda v x) = Lambda v $ optimize x
        optimize' True (Apply x y) = optimize' False $ optimize x `Apply` optimize y
        optimize' _ x = x

link :: String -- entrypoint
    -> [(String, ExprP e)] -- definitions
    -> Maybe (ExprP e)
link entrypoint defs = link' (filter ((/="main") . fst) defs) <$> lookup "main" defs 
    where
        link' ((n, x):xs) e = link' xs (replace n x e)
        link' [] e = e