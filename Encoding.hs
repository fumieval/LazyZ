module LazyZ.Encoding where

import Control.Arrow ((>>>), (<<<))
import Data.Char (chr, ord)
import qualified Codec.Binary.UTF8.String as UTF8

import LazyZ.Expr
import qualified LazyZ.Numbers as N

encodeNum :: (Eq a, Num a) => a -> Expr e
encodeNum 256 = S :$ I :$ I :$ (S :$ I :$ I :$ encodeNum 2)
encodeNum 1 = I
encodeNum 0 = K :$ I
encodeNum n = S :$ (S :$ (K :$ S) :$ K) :$ encodeNum (n - 1)

encodeNat = N.encode

data Decoder a = Accumlator (a -> a) | Value a

decodeBy :: (a -> a) -> a -> Expr e -> Maybe a
decodeBy f x e = case fmap undefined e `apply'` Extern (Accumlator f) `apply'` Extern (Value x) of
    Extern (Value v) -> Just v
    _ -> Nothing
    where
        apply' = applyEx decoder
        decoder (Accumlator f) (Value x) = Extern $ Value (f x)
        decoder f g = Extern f :$ Extern g

decodeNum :: Num a => Expr e -> a
decodeNum = maybe (error "result was a not number") id . decodeBy (+1) 0

fromList :: [Expr e] -> Expr e
fromList (x:xs) = S :$ (S :$ I :$ (K :$ x)) :$ (K :$ fromList xs)
fromList [] = K :$ K

toList :: Expr e -> [Expr e]
toList (K :$ K) = []
toList expr = apply expr K : toList (apply expr (K :$ I))

fromString :: String -> Expr e
fromString = UTF8.encode >>> map (encodeNat . toInteger . fromEnum) >>> fromList

fromString' :: String -> Expr e
fromString' = map (encodeNat . toInteger . fromEnum) >>> fromList

toString :: Expr e -> String
toString = UTF8.decode <<< map decodeNum <<< toList
