module Language.LazyK.Syntax where
import Data.Combinator.Expr

import Control.Applicative (empty ,pure, some, (<$>), (*>), (<*>), (<*))
import Text.Parsec
import Text.Parsec.String

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

ccParser :: Read e => Parser (Expr e)
ccParser = foldl (:$) <$> term <*> many term where
    term = char '(' *> ccParser <* char ')'
        <|> char 'S' *> pure S
        <|> char 'K' *> pure K
        <|> char 'I' *> pure I
        <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
        <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')
