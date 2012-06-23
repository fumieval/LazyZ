-----------------------------------------------------------------------------
-- |
-- Module      :  LazyZ.Expr
-- Copyright   :  (c) fumieval
-- License     :  BSD-style
-- 
-- Maintainer  :  @fumieval
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions on Church encoding.
--
-----------------------------------------------------------------------------

module LazyZ.Combinator where

import Prelude ()
import LazyZ.Expr

false = K :$ I
true = K

fix :: Expr e -> Expr e
fix f = S :$ I :$ I :$ (S :$ (K :$ f) :$ (S :$ I :$ I))

succ :: Expr e -> Expr e
succ x = S :$ (S :$ (K :$ S) :$ K) :$ x

(+) :: Expr e -> Expr e -> Expr e
x + y = x :$ (S :$ (S :$ (K :$ S) :$ K)) :$ y

(*) :: Expr e -> Expr e -> Expr e
x * y = S :$ (K :$ x) :$ y

(.) = (*)

(^) :: Expr e -> Expr e -> Expr e
x ^ y = y :$ x