Lazy Z (ver0.1) by @fumieval

# Lazy Zとは

Lazy Zは、Lazy Kの「組み込み関数がS、K、Iの3つしかない」という特長を継承しつつ、文字列・数値リテラル、ラムダ式を導入した言語です。

# 構文
    main input = cons (f input) str;  -- 関数定義、ラムダ式、行コメント
    f = \x -> x 0; -- ラムダ式、数値リテラル
    cons x y = \f -> f x y; {- ブロックコメント -}
    str = (++) "Hello, world!" str; -- 文字列リテラル、再帰的定義

# ビルド
$ ghc -o LazyZ -O2 Main.hs LazyK.hs Combinator.hs Encoding.hs Expr.hs Link.hs Program.hs Numbers.hs Syntax.hs
    
# コマンド
* LazyZ (build|execute|run) [input file(s)]
    * LazyZ build [files] ファイルをコンパイルし、Lazy Kのコードを出力します。
    * LazyZ execute [files] Lazy Zのプログラムを実行します。
    * LazyZ run [file] Lazy Kのプログラムを実行します。 
            
# 実装中の機能
* パターンマッチ
* オフサイドルール
* where節
* 中置演算子
* 最適化