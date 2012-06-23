{-# LANGUAGE Rank2Types #-}
module LazyZ.Link where

import Control.Applicative
import Data.List as L
import qualified Data.Map as Map
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
    -> Map.Map String (ExprP e) -- definitions
    -> Maybe (ExprP e)
link entrypoint defs = link' <$> Map.lookup "main" defs <*> pure (Map.toList $ Map.delete "main" defs)
    where
        link' e ((n, x):xs) = link' (replace n x e) xs
        link' e [] = e