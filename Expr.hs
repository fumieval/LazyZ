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

equiv :: Expr e -> Expr e -> Bool
equiv f g = fmap (const ()) f == fmap (const ()) g

isPrim S = True
isPrim K = True
isPrim I = True
isPrim _ = False

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

-- | The 'applyEx' function grants 'apply' the binary operation between external values. 
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
simpl (I :$ x) = simpl x
simpl (K :$ x :$ y) = simpl x
simpl (S :$ (K :$ x) :$ y :$ z) = simpl (simpl x :$ (simpl y :$ simpl z))
simpl (S :$ x :$ (K :$ y) :$ z) = simpl (simpl x :$ simpl z :$ simpl y)
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
-- refered to John Tromp "Binary Lambda Calculus and Combinatory Logic", 2011 (http://homepages.cwi.nl/~tromp/cl/LC.pdf section 3.2)
bindee _ (S :$ K :$ _) = S :$ K
bindee x f              | x `notElem` frees f = K :$ f
bindee x (Free x')      | x == x' = I
bindee x (f :$ Free x') | x == x' = f
bindee x (Free y :$ f :$ Free z)
    | x == y && x == z = bindee x $ S :$ S :$ K :$ Free x :$ f
bindee x (f :$ (g :$ h))
    | isPrim f && isPrim g = bindee x $ S :$ bindee x f :$ g :$ h
bindee x ((f :$ g) :$ h)
    | isPrim f && isPrim h = bindee x $ S :$ f :$ bindee x h :$ g
bindee x ((f :$ g) :$ (h :$ g'))
    | isPrim f && isPrim h && equiv g g' = bindee x $ S :$ f :$ h :$ g
bindee x (f :$ g) = S :$ bindee x f :$ bindee x g

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

-- -------------------------------------------------------
-- Partial Evaluations
{- Staticの中はコンパイル時に評価するが、その中にあったとしてもRuntimeは評価しない。 -}

data Eval e = Term (Expr e) -- 適用を含まない項
                | Runtime (Eval e) -- 必ず実行時に評価される項
                | Static (Eval e) -- コンパイル時に評価される項
                | Undecided (Eval e) -- 特に指定がない項
                | App (Eval e) (Eval e) -- 適用

lift :: (Expr e -> Expr e) -> Eval e -> Eval e
lift f (App x y) = Undecided (lift f x) `App` Undecided (lift f y)
lift f (Runtime x) = Runtime $ lift f x
lift f (Static x) = Static $ lift f x
lift f (Undecided x) = Undecided $ lift f x
lift f (Term x) = Term $ f x

flatten :: Eval e -> Expr e
flatten (Term x) = x
flatten (Runtime x) = flatten x
flatten (Static x) = flatten x
flatten (Undecided x) = flatten x
flatten (App x y) = flatten x :$ flatten y

partialEval = eval' False where
    eval' _ (Term x) = x
    eval' _ (Runtime x) = flatten x
    eval' _ (Static x) = eval' True x
    eval' _ (Undecided x) = eval' False x
    eval' False (App x y) = eval' False x :$ eval' False y
    eval' True (App x y) = eval' True x `apply` eval' True y
