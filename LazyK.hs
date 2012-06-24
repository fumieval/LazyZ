module LazyZ.LazyK where

import Control.Applicative (empty ,pure, some, (<$>), (*>), (<*>), (<*))
import Control.Arrow
import Text.Parsec
import Text.Parsec.String
import LazyZ.Expr
import LazyZ.Encoding
import Data.Maybe (listToMaybe)
import Data.Char (chr, ord)

unlambdaParser :: Read e => Parser (Expr e)
unlambdaParser = char '`' *> ((:$) <$> unlambdaParser <*> unlambdaParser)
    <|> char 's' *> pure S
    <|> char 'k' *> pure K
    <|> char 'i' *> pure I
    <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
    <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')

runLazyK :: (Expr e) -> String -> String
runLazyK expr = fst . decode . eval . (expr :$) . encode
    where
        encode = fromList . map encodeNat . (++ repeat 256) . map ord
        decode = toList >>> map decodeNat >>> span (<256) >>> map chr *** listToMaybe