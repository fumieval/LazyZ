
simpl :: Expr e -> Expr e
simpl (I :$ x) = simpl x
simpl (K :$ x :$ y) = simpl x
simpl (S :$ (K :$ x) :$ y :$ z) = simpl (simpl x :$ (simpl y :$ simpl z))
simpl (S :$ x :$ (K :$ y) :$ z) = simpl (simpl x :$ simpl z :$ simpl y)
simpl (f :$ g) = simpl f :$ simpl g
simpl x = x 

evalEx :: (e -> e -> e) -> Expr e -> Expr e
evalEx t (f :$ g) = applyEx t (evalEx t f) (evalEx t g)
evalEx t x = x

data DecoratedExprP e = Wrap (ExprP e) | CompileTime (DecoratedExprP e)
                        deriving (Show, Eq)
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
