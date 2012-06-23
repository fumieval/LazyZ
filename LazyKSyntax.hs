module LazyZ.LazyKSyntax where

import Control.Applicative (empty ,pure, some, (<$>), (*>), (<*>), (<*))
import Text.Parsec
import Text.Parsec.String
import LazyZ.Expr

unlambdaParser :: Read e => Parser (Expr e)
unlambdaParser = char '`' *> ((:$) <$> unlambdaParser <*> unlambdaParser)
    <|> char 's' *> pure S
    <|> char 'k' *> pure K
    <|> char 'i' *> pure I
    <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
    <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')