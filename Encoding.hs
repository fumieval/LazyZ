module LazyZ.Encoding where

import LazyZ.Expr
import Data.Char (chr, ord)

import qualified LazyZ.Numbers as N

fromList :: [Expr e] -> Expr e
fromList (x:xs) = S :$ (S :$ I :$ (K :$ x)) :$ (K :$ fromList xs)
fromList [] = K :$ K

encodeNat :: Int -> Expr e
encodeNat 32 = S :$ (K :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)) :$ (S :$ (S :$ I :$ I) :$ I :$ (S :$ (S :$ (K :$ S) :$ K) :$ I))
encodeNat 64 = S :$ (S :$ (S :$ (K :$ S) :$ K)) :$ (S :$ I :$ I) :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)
encodeNat 128 = S :$ (K :$ (S :$ (S :$ (K :$ S) :$ K) :$ I)) :$ (S :$ (S :$ (S :$ (K :$ S) :$ K)) :$ (S :$ I :$ I) :$ (S :$ (S :$ (K :$ S) :$ K) :$ I))
encodeNat 1 = I
encodeNat 0 = K :$ I
encodeNat n = S :$ (S :$ (K :$ S) :$ K) :$ encodeNat (n - 1)

encodeNat' = N.encode

fromString :: String -> Expr e
fromString = fromList . map (encodeNat . ord)

fromString' :: String -> Expr e
fromString' = fromList . map (encodeNat' . toInteger . ord)

data ChurchDecode = Increment | Value {-# UNPACK #-} !Int deriving (Show)
decoder Increment (Value n) = Value (n + 1)
decoder _ _ = error "decoding error"

decodeNat :: Expr e -> Int
decodeNat e = case fmap undefined e `apply'` Extern Increment `apply'` Extern (Value 0) of
                   Extern (Value n) -> n
                   x -> error $ "result was a not number"
    where
        apply' = applyEx decoder

toList :: Expr e -> [Expr e]
toList (K :$ K) = []
toList expr = apply expr K : toList (apply expr (K :$ I))

toString :: Expr e -> String
toString = map (chr . decodeNat) . toList
