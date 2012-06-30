module LazyZ.LazyK where

import Control.Applicative (empty ,pure, some, (<$>), (*>), (<*>), (<*))
import Control.Arrow
import Text.Parsec
import Text.Parsec.String
import LazyZ.Expr
import LazyZ.Encoding

import qualified Codec.Binary.UTF8.String as UTF8

import Data.Maybe (listToMaybe)
import Data.Char (chr, ord)

showUnlambda :: Show e => Expr e -> String
showUnlambda I = "i"
showUnlambda K = "k"
showUnlambda S = "s"
showUnlambda (Free x) = "[" ++ x ++ "]"
showUnlambda (Extern e) = "<" ++ show e ++ ">"
showUnlambda (a :$ b) = "`" ++ showUnlambda a ++ showUnlambda b

unlambdaParser :: Read e => Parser (Expr e)
unlambdaParser = char '`' *> ((:$) <$> unlambdaParser <*> unlambdaParser)
    <|> char 's' *> pure S
    <|> char 'k' *> pure K
    <|> char 'i' *> pure I
    <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
    <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')

runLazyK :: (Expr e) -> String -> String
runLazyK expr = fst . decode . eval . (expr :$) . flip (curry encode) (repeat 256)
    where
        encode = fromList <<< map encodeNum <<< uncurry (++) <<< first (map fromEnum <<< UTF8.encode)
        decode = toList   >>> map decodeNum >>> span (<256)  >>> first (map toEnum   >>> UTF8.decode)