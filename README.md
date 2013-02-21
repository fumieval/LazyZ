Lazy Z (ver0.3) by @fumieval

# Lazy Zとは

Lazy Zは、Lazy Kの「組み込み関数がS、K、Iの3つしかない」という特長を継承しつつ、文字列・数値リテラル、ラムダ式を導入した言語です。

# 構文
    infixr 6 :;
    infixr 6 ++;

    main input = 42 : str;  -- 関数定義、数値リテラル、行コメント
    
    (:) x xs = \f -> f x xs; {- ラムダ式、ブロックコメント -}
    nil = \f -> \x y -> y

    str = "Hello, world!" (:) nil ++ str; -- 文字列リテラル、再帰的定義