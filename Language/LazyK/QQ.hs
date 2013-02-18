{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.LazyK.QQ where

import Control.Applicative
import Text.Parsec (parse)
import Data.Combinator.Expr
import Language.LazyK.Syntax
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Void

toExp :: Expr Void -> ExpQ
toExp S = [|S|]
toExp K = [|K|]
toExp I = [|I|]
toExp (Free v) = [|Free v|]
toExp (f :$ g) = uInfixE (parensE $ toExp f) (conE '(:$)) (parensE $ toExp g)

cc :: QuasiQuoter
cc = QuasiQuoter { quoteExp = either (fail.show) toExp . parse ccParser "" }