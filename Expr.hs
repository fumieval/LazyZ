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
-- The combinatory expressions, and associated operations.
--
-----------------------------------------------------------------------------

module LazyZ.Expr where
import Prelude hiding (length)
import Data.List (minimumBy)
import Data.Function (on)

data Expr e = I | K | S | Expr e :$ Expr e | Free String | Extern e deriving (Show, Eq, Ord)

instance Functor Expr where
    fmap f (Extern x) = Extern $ f x
    fmap f (x :$ y) = fmap f x :$ fmap f y
    fmap _ I = I
    fmap _ K = K
    fmap _ S = S
    fmap _ (Free x) = Free x

instance Monad Expr where
    return = Extern
    Extern x >>= f = f x
    (a :$ b) >>= f = (a >>= f) :$ (b >>= f)
    e >>= f = fmap undefined e

-- Basic Functions
-- | The 'length' function returns a length of an expression. 
length :: Expr e -> Int
length (f :$ g) = length f + length g + 1
length _ = 1

internalize :: Expr (Expr e) -> Expr e
internalize (f :$ g) = internalize f :$ internalize g
internalize (Extern f) = f

-- -------------------------------------------------------
-- Applying and evaluation
-- | The 'apply' function expresses beta reduction.
-- Note that it may not stop, for example, apply (S :$ I :$ I) (S :$ I :$ I).
apply :: Expr e -> Expr e -> Expr e
apply I x = x
apply (K :$ x) y = x
apply (S :$ x :$ y) z = apply x z `apply` apply y z
apply f g = f :$ g

-- | The 'eval' function evaluates an expression. 
eval :: Expr e -> Expr e
eval (f :$ g) = eval f `apply` eval g
eval x = x

applyEx :: (e -> e -> e) -> Expr e -> Expr e -> Expr e
applyEx t I x = x
applyEx t (K :$ x) y = x
applyEx t (S :$ x :$ y) z = applyEx t (applyEx t x z) (applyEx t y z)
applyEx t (Extern x) (Extern y) = Extern $ t x y
applyEx t f g = f :$ g

evalEx :: (e -> e -> e) -> Expr e -> Expr e
evalEx t (f :$ g) = applyEx t (evalEx t f) (evalEx t g)
evalEx t x = x

simpl :: Expr e -> Expr e
simpl (I :$ x) = x
simpl (K :$ x :$ y) = x
simpl (S :$ (K :$ x) :$ y :$ z) = simpl (x :$ (y :$ z))
simpl (S :$ x :$ (K :$ y) :$ z) = simpl (x :$ z :$ y)
simpl (f :$ g) = simpl f :$ simpl g
simpl x = x

-- -------------------------------------------------------
-- Variables and bindings

-- | The 'frees' function returns a list of free variables in the expression.
frees :: Expr e -> [String]
frees (Free x) = [x]
frees (f :$ g) = frees f ++ frees g
frees _ = []

-- | The 'subst' function binds specified variable to its argument.
subst :: String -- free variable to search for
    -> Expr e -- replacement expression
    -> Expr e -> Expr e
subst v r (f :$ g) = subst v r f :$ subst v r g
subst v r (Free v') | v == v' = r
subst _ _ e = e

-- | The 'bindee' function transforms an expression to a combinator which binds specified variable when it is applied.
bindee :: String -> Expr e -> Expr e
bindee p e | p `notElem` frees e = K :$ e
bindee p (f :$ g) = S :$ bindee p f :$ bindee p g
bindee p (Free p') | p == p' = I

-- -------------------------------------------------------
-- Representations

-- | The 'showUnlambda' represents an expression as Unlambda style.
showUnlambda :: Show e => Expr e -> String
showUnlambda I = "i"
showUnlambda K = "k"
showUnlambda S = "s"
showUnlambda (Free x) = "[" ++ x ++ "]"
showUnlambda (Extern e) = "<" ++ show e ++ ">"
showUnlambda (a :$ b) = "`" ++ showUnlambda a ++ showUnlambda b
