module LazyZ.Encoding where

import Control.Arrow
import LazyZ.Expr
import Data.Char (chr, ord)

import Data.Word (Word8)
import qualified Codec.Binary.UTF8.String as UTF8

import qualified LazyZ.Numbers as N

encodeNum :: (Eq a, Num a) => a -> Expr e
encodeNum 32 = S :$ (K :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)) :$ (S :$ (S :$ I :$ I) :$ I :$ (S :$ (S :$ (K :$ S) :$ K) :$ I))
encodeNum 64 = S :$ (S :$ (S :$ (K :$ S) :$ K)) :$ (S :$ I :$ I) :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)
encodeNum 128 = S :$ (K :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)) :$ (S :$ (S :$ (S :$ (K :$ S) :$ K)) :$ (S :$ I :$ I) :$ (S :$ (S :$ (K :$ S) :$ K) :$ I))
encodeNum 256 = S :$ I :$ I :$ (S :$ I :$ I :$ encodeNum 2)
encodeNum 1 = I
encodeNum 0 = K :$ I
encodeNum n = S :$ (S :$ (K :$ S) :$ K) :$ encodeNum (n - 1)

encodeNat' = N.encode

data Decoder a = Accumlator (a -> a) | Value a

decoder (Accumlator f) (Value x) = Value (f x)
decoder _ _ = error "decoding error"

decodeNum :: Num a => Expr e -> a
decodeNum e = case fmap undefined e `app` Extern (Accumlator (1+)) `app` Extern (Value 0) of
                   Extern (Value n) -> n
                   x -> error $ "result was a not number"
    where
        app = applyEx decoder

fromList :: [Expr e] -> Expr e
fromList (x:xs) = S :$ (S :$ I :$ (K :$ x)) :$ (K :$ fromList xs)
fromList [] = K :$ K

toList :: Expr e -> [Expr e]
toList (K :$ K) = []
toList expr = apply expr K : toList (apply expr (K :$ I))

fromString :: String -> Expr e
fromString = UTF8.encode >>> map encodeNum >>> fromList

toString :: Expr e -> String
toString = UTF8.decode <<< map decodeNum <<< toList
