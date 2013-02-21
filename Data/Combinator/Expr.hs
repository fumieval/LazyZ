-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Combinator.Expr
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

module Data.Combinator.Expr where
import Prelude hiding (length)
import Data.List (minimumBy)
import Data.Function (on)

infixl 9 :$

data Expr e = Expr e :$ Expr e -- application
            | I | K | S -- primitive combinators
            | Free String -- free variable
            | Atom (Expr e)
            | Extern e -- external value
              deriving (Show, Eq, Ord)

instance Functor Expr where
    fmap f x = x >>= return . f

instance Monad Expr where
    return = Extern
    Extern x >>= f = f x
    (a :$ b) >>= f = (a >>= f) :$ (b >>= f)
    I >>= _ = I
    K >>= _ = K
    S >>= _ = S
    Free v >>= _ = Free v

-- Basic Functions
-- | The 'length' function returns a length of an expression. 
length :: Expr e -> Int
length (f :$ g) = length f + length g + 1
length _ = 1

-- | The 'isPrim' function returns True iff the argument is primitive combinator.
isPrim S = True
isPrim K = True
isPrim I = True
isPrim x = False

simpl (I :$ x) = simpl x
simpl (K :$ x :$ y) = simpl x
simpl (S :$ x :$ y :$ z)
    | length c < length d = c
    | otherwise = d
    where
        x' = simpl x
        y' = simpl y
        z' = simpl z
        c = S :$ x' :$ y' :$ z'
        d = simpl (x' :$ z') :$ simpl (y' :$ z')
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
bindee :: Eq e => String -> Expr e -> Expr e
-- refered to John Tromp "Binary Lambda Calculus and Combinatory Logic", 2011 (http://homepages.cwi.nl/~tromp/cl/LC.pdf section 3.2)
bindee _ (S :$ K :$ _) = S :$ K
bindee x f              | x `notElem` frees f = K :$ f
bindee x (Free x')      | x == x' = I
bindee x (f :$ Free x') | x `notElem` frees f && x == x' = f
bindee x (Free y :$ f :$ Free z)
    | x == y && x == z = bindee x $ S :$ S :$ K :$ Free x :$ f
bindee x (f :$ (g :$ h))
    | isPrim f && isPrim g = bindee x $ S :$ bindee x f :$ g :$ h
bindee x ((f :$ g) :$ h)
    | isPrim f && isPrim h = bindee x $ S :$ f :$ bindee x h :$ g
bindee x ((f :$ g) :$ (h :$ g'))
    | isPrim f && isPrim h && g == g' = bindee x $ S :$ f :$ h :$ g
bindee x (f :$ g) = S :$ bindee x f :$ bindee x g

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
eval (Atom x) = x
eval (f :$ g) = eval f `apply` eval g
eval x = x

-- | The 'applyEx' function grants 'apply' the binary operation between external values. 
applyEx :: (e -> e -> Expr e) -> Expr e -> Expr e -> Expr e
applyEx t I x = x
applyEx t (K :$ x) y = x
applyEx t (S :$ x :$ y) z = applyEx t (applyEx t x z) (applyEx t y z)
applyEx t (Extern x) (Extern y) = t x y
applyEx t f g = f :$ g
