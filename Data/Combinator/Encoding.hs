{-# LANGUAGE QuasiQuotes, FlexibleInstances #-}
module Data.Combinator.Encoding where

import Data.Combinator.Expr
import Language.LazyK.QQ
import qualified Data.Map as Map
import Data.Void

newtype Church a = Church {getChurch :: a}
newtype Scott a = Scott {getScott :: a}

class Encodable a where
    encode :: a -> Expr Void

instance Encodable (Church Integer) where
    encode (Church n) = case Map.lookup n numbers of
        Just x -> x
        Nothing
            | mod n 2 == 0 -> n'
            | otherwise -> S :$ (S :$ (K :$ S) :$ K) :$ n'
        where
            n' = S :$ (K :$ (numbers Map.! 2)) :$ encode (Church $ div n 2)

instance Encodable (Scott Integer) where
    encode (Scott 0) = K :$ I
    encode (Scott n) = S :$ (K :$ K) :$ (S :$ I :$ (K :$ encode (Scott $ pred n)))

instance Encodable a => Encodable (Scott [a]) where
    encode (Scott xs) = foldr cons empty (map encode xs) where
        empty = K :$ I
        cons x xs = S :$ (S :$ I :$ (K :$ x)) :$ (K :$ xs)
instance Encodable a => Encodable (Church [a]) where
    encode (Church xs) = foldr cons empty (map encode xs) where
        empty = K :$ I
        cons x xs = S :$ (S :$ (K :$ S) :$ (S :$ (K :$ K) :$ (S :$ I :$ (K :$ x)))) :$ xs

class Decodable a where
    decode :: Expr Void -> Maybe a

instance Decodable Integer where
    decode = decodeBy succ 0

data Decoder a = Accumlator (a -> a) | Value a

decodeBy :: (a -> a) -> a -> Expr e -> Maybe a
decodeBy f x e = case fmap undefined (eval e) `app` Extern (Accumlator f) `app` Extern (Value x) of
    Extern (Value v) -> Just v
    _ -> Nothing
    where
        app = applyEx decoder
        decoder (Accumlator f) (Value x) = Extern $ Value (f x)
        decoder f g = Extern f :$ Extern g

numbers :: Map.Map Integer (Expr Void)
numbers = Map.union es bs where
    es = Map.fromList [
        (0, [cc|KI|])
        ,(1, [cc|I|])
        ,(2, S :$ (S :$ (K :$ S) :$ K) :$ (numbers Map.! 1))
        ,(4, [cc|SII|] :$ (numbers Map.! 2))
        ,(16, S :$ (S :$ I :$ I) :$ I :$ (numbers Map.! 2))
        ,(256, [cc|SII|] :$ (numbers Map.! 4))
        ]
    bs = Map.map (:$([cc|S(KS)K|])) $ Map.fromList [
        (0,[cc|K(KI)|])
        ,(1,[cc|KI|])
        ,(2,[cc|SS(KI)|])
        ,(3,[cc|SS(SS(KI))|])
        ,(4,[cc|SSI(SS(KI))|])
        ,(5,[cc|SS(SSI(SS(KI)))|])
        ,(6,[cc|S(SSS)(SS(KI))|])
        ,(7,[cc|SS(S(SSS)(SS(KI)))|])
        ,(8,[cc|S(S(KS)(SS))I(SS(KI))|])
        ,(9,[cc|SS(SS)(SS(KI))|])
        ,(10,[cc|SS(SS(SS)(SS(KI)))|])
        ,(11,[cc|SS(SS(SS(SS)(SS(KI))))|])
        ,(12,[cc|S(SSS)(SS(SS(KI)))|])
        ,(13,[cc|SS(S(SSS)(SS(SS(KI))))|])
        ,(14,[cc|SS(SS(S(SSS)(SS(SS(KI)))))|])
        ,(15,[cc|SS(SS(SS(S(SSS)(SS(SS(KI))))))|])
        ,(16,[cc|SS(SSI)(SS(KI))|])
        ,(17,[cc|SS(SS(SSI)(SS(KI)))|])
        ,(18,[cc|SS(SS(SS(SSI)(SS(KI))))|])
        ,(19,[cc|SS(SS(SS(SS(SSI)(SS(KI)))))|])
        ,(20,[cc|S(SSS)(SSI(SS(KI)))|])
        ,(21,[cc|SS(S(SSS)(SSI(SS(KI))))|])
        ,(22,[cc|SS(SS(S(SSS)(SSI(SS(KI)))))|])
        ,(23,[cc|SS(SS(SS(S(SSS)(SSI(SS(KI))))))|])
        ,(24,[cc|S(SI(SS(KI)))(S(SSS)(SS(SS(KI))))|])
        ,(25,[cc|S(SS(KI))(SS(SSI(SS(KI))))|])
        ,(26,[cc|SS(S(SS(KI))(SS(SSI(SS(KI)))))|])
        ,(27,[cc|SSI(SS(SS(KI)))|])
        ,(28,[cc|SS(SSI(SS(SS(KI))))|])
        ,(29,[cc|SS(SS(SSI(SS(SS(KI)))))|])
        ,(30,[cc|S(SSS)(SS(SSI(SS(KI))))|])
        ,(31,[cc|SS(S(SSS)(SS(SSI(SS(KI)))))|])
        ,(32,[cc|S(SS(SSI(SS(KI))))(SS(KI))|])
        ,(33,[cc|SS(S(SS(SSI(SS(KI))))(SS(KI)))|])
        ,(34,[cc|S(SI(SS(KI)))(SS(SS(SSI)(SS(KI))))|])
        ,(35,[cc|SS(S(SI(SS(KI)))(SS(SS(SSI)(SS(KI)))))|])
        ,(36,[cc|SS(S(SSS))(SS(KI))|])
        ,(37,[cc|SS(SS(S(SSS))(SS(KI)))|])
        ,(38,[cc|SS(SS(SS(S(SSS))(SS(KI))))|])
        ,(39,[cc|SS(SS(SS(SS(S(SSS))(SS(KI)))))|])
        ,(40,[cc|S(SI(SS(KI)))(S(SSS)(SSI(SS(KI))))|])
        ,(41,[cc|SS(S(SI(SS(KI)))(S(SSS)(SSI(SS(KI)))))|])
        ,(42,[cc|S(SSS)(S(SSS)(SS(KI)))|])
        ,(43,[cc|SS(S(SSS)(S(SSS)(SS(KI))))|])
        ,(44,[cc|SS(SS(S(SSS)(S(SSS)(SS(KI)))))|])
        ,(45,[cc|SS(SS(SS(S(SSS)(S(SSS)(SS(KI))))))|])
        ,(46,[cc|SS(SS(SS(SS(S(SSS)(S(SSS)(SS(KI)))))))|])
        ,(47,[cc|SS(SS(SS(SS(SS(S(SSS)(S(SSS)(SS(KI))))))))|])
        ,(48,[cc|S(SI(SS(SS(KI))))(SS(SSI)(SS(KI)))|])
        ,(49,[cc|S(SS(KI))(SS(S(SSS)(SS(KI))))|])
        ,(50,[cc|SS(S(SS(KI))(SS(S(SSS)(SS(KI)))))|])
        ,(51,[cc|SS(SS(S(SS(KI))(SS(S(SSS)(SS(KI))))))|])
        ,(52,[cc|SS(SS(SS(S(SS(KI))(SS(S(SSS)(SS(KI)))))))|])
        ,(53,[cc|SS(SS(SS(SS(S(SS(KI))(SS(S(SSS)(SS(KI))))))))|])
        ,(54,[cc|S(SI(SS(KI)))(SSI(SS(SS(KI))))|])
        ,(55,[cc|SS(S(SI(SS(KI)))(SSI(SS(SS(KI)))))|])
        ,(56,[cc|S(SSS)(SS(S(SSS)(SS(KI))))|])
        ,(57,[cc|SS(S(SSS)(SS(S(SSS)(SS(KI)))))|])
        ,(58,[cc|SS(SS(S(SSS)(SS(S(SSS)(SS(KI))))))|])
        ,(59,[cc|SS(SS(SS(S(SSS)(SS(S(SSS)(SS(KI)))))))|])
        ,(60,[cc|S(SI(SS(KI)))(S(SSS)(SS(SSI(SS(KI)))))|])
        ,(61,[cc|SS(S(SI(SS(KI)))(S(SSS)(SS(SSI(SS(KI))))))|])
        ,(62,[cc|S(SI(SS(KI)))(SS(S(SSS)(SS(SSI(SS(KI))))))|])
        ,(63,[cc|S(SI(SS(S(SSS)(SS(KI)))))(SS(SS)(SS(KI)))|])
        ,(64,[cc|S(S(SSS)(SS(KI)))(SS(KI))|])
        ,(65,[cc|SS(S(S(SSS)(SS(KI)))(SS(KI)))|])
        ,(66,[cc|SS(SS(S(S(SSS)(SS(KI)))(SS(KI))))|])
        ,(67,[cc|SS(SS(SS(S(S(SSS)(SS(KI)))(SS(KI)))))|])
        ,(68,[cc|S(SI(SSI(SS(KI))))(SS(SS(SSI)(SS(KI))))|])
        ,(69,[cc|SS(S(SI(SSI(SS(KI))))(SS(SS(SSI)(SS(KI)))))|])
        ,(70,[cc|S(SI(SS(S(SSS)(SS(KI)))))(SS(SS(SS)(SS(KI))))|])
        ,(71,[cc|SS(S(SI(SS(S(SSS)(SS(KI)))))(SS(SS(SS)(SS(KI)))))|])
        ,(72,[cc|S(SSS)(S(S(KS)(SS))I(SS(KI)))|])
        ,(73,[cc|SS(S(SSS)(S(S(KS)(SS))I(SS(KI))))|])
        ,(74,[cc|S(SI(SS(KI)))(SS(SS(S(SSS))(SS(KI))))|])
        ,(75,[cc|SS(S(SI(SS(KI)))(SS(SS(S(SSS))(SS(KI)))))|])
        ,(76,[cc|S(SI(SS(KI)))(SS(SS(SS(S(SSS))(SS(KI)))))|])
        ,(77,[cc|SS(S(SI(SS(KI)))(SS(SS(SS(S(SSS))(SS(KI))))))|])
        ,(78,[cc|S(SI(SS(KI)))(SS(SS(SS(SS(S(SSS))(SS(KI))))))|])
        ,(79,[cc|SS(S(SI(SS(KI)))(SS(SS(SS(SS(S(SSS))(SS(KI)))))))|])
        ,(80,[cc|S(SI(SSI(SS(KI))))(S(SSS)(SSI(SS(KI))))|])
        ,(81,[cc|SS(SS(SS))(SS(KI))|])
        ,(82,[cc|SS(SS(SS(SS))(SS(KI)))|])
        ,(83,[cc|SS(SS(SS(SS(SS))(SS(KI))))|])
        ,(84,[cc|SS(SS(SS(SS(SS(SS))(SS(KI)))))|])
        ,(85,[cc|SS(SS(SS(SS(SS(SS(SS))(SS(KI))))))|])
        ,(86,[cc|SS(SS(SS(SS(SS(SS(SS(SS))(SS(KI)))))))|])
        ,(87,[cc|S(S(S(SSS)(SS(KI)))S)(SS(SS(SS))(SS(KI)))|])
        ,(88,[cc|S(S(S(SSS)(SS(KI)))S)(SS(SS(SS(SS))(SS(KI))))|])
        ,(89,[cc|S(S(S(S(KS)(SS))I(SS(KI)))S)(SS(SS(SS))(SS(KI)))|])
        ,(90,[cc|S(SSS)(SS(SS)(SS(KI)))|])
        ,(91,[cc|SS(S(SSS)(SS(SS)(SS(KI))))|])
        ,(92,[cc|SS(SS(S(SSS)(SS(SS)(SS(KI)))))|])
        ,(93,[cc|SS(SS(SS(S(SSS)(SS(SS)(SS(KI))))))|])
        ,(94,[cc|SS(SS(SS(SS(S(SSS)(SS(SS)(SS(KI)))))))|])
        ,(95,[cc|SS(SS(SS(SS(SS(S(SSS)(SS(SS)(SS(KI))))))))|])
        ,(96,[cc|S(SI(S(SSS)(SS(KI))))(SS(SSI)(SS(KI)))|])
        ,(97,[cc|S(S(SS(SSI)(SS(KI)))S)(SS(SS(SS))(SS(KI)))|])
        ,(98,[cc|S(SI(SS(KI)))(S(SS(KI))(SS(S(SSS)(SS(KI)))))|])
        ,(99,[cc|S(S(SS(SS)(SS(KI)))S)(S(SSS)(SS(SS)(SS(KI))))|])
        ,(100,[cc|S(SS(KI))(SS(SS(SS)(SS(KI))))|])
        ,(101,[cc|SS(S(SS(KI))(SS(SS(SS)(SS(KI)))))|])
        ,(102,[cc|SS(SS(S(SS(KI))(SS(SS(SS)(SS(KI))))))|])
        ,(103,[cc|SS(SS(SS(S(SS(KI))(SS(SS(SS)(SS(KI)))))))|])
        ,(104,[cc|SS(SS(SS(SS(S(SS(KI))(SS(SS(SS)(SS(KI))))))))|])
        ,(105,[cc|S(SI(SS(SSI(SS(KI)))))(SS(S(SSS)(SSI(SS(KI)))))|])
        ,(106,[cc|S(S(SS(SSI)(SS(KI)))S)(S(SSS)(SS(SS)(SS(KI))))|])
        ,(107,[cc|S(S(SS(SSI)(SS(KI)))S)(SS(S(SSS)(SS(SS)(SS(KI)))))|])
        ,(108,[cc|S(SI(SS(SS(KI))))(SS(S(SSS))(SS(KI)))|])
        ,(109,[cc|SS(S(SI(SS(SS(KI))))(SS(S(SSS))(SS(KI))))|])
        ,(110,[cc|S(SSS)(SS(SS(SS)(SS(KI))))|])
        ,(111,[cc|SS(S(SSS)(SS(SS(SS)(SS(KI)))))|])
        ,(112,[cc|SS(SS(S(SSS)(SS(SS(SS)(SS(KI))))))|])
        ,(113,[cc|SS(SS(SS(S(SSS)(SS(SS(SS)(SS(KI)))))))|])
        ,(114,[cc|SS(SS(SS(SS(S(SSS)(SS(SS(SS)(SS(KI))))))))|])
        ,(115,[cc|SS(SS(SS(SS(SS(S(SSS)(SS(SS(SS)(SS(KI)))))))))|])
        ,(116,[cc|S(SI(SSI(SS(KI))))(SS(SS(SSI(SS(SS(KI))))))|])
        ,(117,[cc|S(S(SS(S(SSS))(SS(KI)))S)(SS(SS(SS))(SS(KI)))|])
        ,(118,[cc|S(S(SS(S(SSS))(SS(KI)))S)(SS(SS(SS(SS))(SS(KI))))|])
        ,(119,[cc|S(SI(SS(S(SSS)(SS(KI)))))(SS(SS(SSI)(SS(KI))))|])
        ,(120,[cc|S(SI(S(SSS)(SS(KI))))(S(SSS)(SSI(SS(KI))))|])
        ,(121,[cc|S(SS(KI))(SS(SS(SS(SS)(SS(KI)))))|])
        ,(122,[cc|SS(S(SS(KI))(SS(SS(SS(SS)(SS(KI))))))|])
        ,(123,[cc|SS(SS(S(SS(KI))(SS(SS(SS(SS)(SS(KI)))))))|])
        ,(124,[cc|SS(SS(SS(S(SS(KI))(SS(SS(SS(SS)(SS(KI))))))))|])
        ,(125,[cc|S(SS(SS(KI)))(SS(SSI(SS(KI))))|])
        ,(126,[cc|SS(S(SS(SS(KI)))(SS(SSI(SS(KI)))))|])
        ,(127,[cc|SS(SS(S(SS(SS(KI)))(SS(SSI(SS(KI))))))|])
        ,(128,[cc|S(SS(S(SSS)(SS(KI))))(SS(KI))|])
        ]