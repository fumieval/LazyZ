module LazyZ.Syntax where

import Control.Applicative (some, (<$>), (*>), (<*>), (<*))
import Data.Char (isSpace, ord)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as T

import LazyZ.Encoding (encodeNat, fromString, fromList)
import LazyZ.Program (ExprP(..))
import LazyZ.Expr (Expr)

data ExtraNotation e = Pure e | Paren [e] | Bracket [e]

data Literal = LNat Integer | LChar Char | LStr String deriving (Show, Eq)

encodeLiteral :: Literal -> Expr e
encodeLiteral (LNat n) = encodeNat n
encodeLiteral (LStr x) = fromString x
encodeLiteral (LChar c) = encodeNat (toInteger $ ord c)

lexer = T.makeTokenParser haskellStyle

identifier = T.identifier lexer <|> parens operator
natural = T.natural lexer
stringLiteral = T.stringLiteral lexer
charLiteral = T.charLiteral lexer
parens = T.parens lexer
whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
operator = T.operator lexer

indentLevel = length . takeWhile isSpace

lazyZparser = some definition

definition :: Parser (String, ExprP Literal)
definition = lexeme $ do
    whiteSpace
    -- halt <- try (char '#') *> return Halt <|> return id
    name <- identifier
    args <- many (whiteSpace *> identifier)
    string "="
    whiteSpace
    definition <- expr
    whiteSpace
    char ';'
    return (name, construct args definition)

construct :: [String] -> ExprP e -> ExprP e
construct (x:xs) e = currying x xs e
construct [] e = e

currying p [] e = Lambda p e
currying p (p':ps) e = Lambda p (currying p' ps e)

lambda :: Parser (ExprP Literal)
lambda = (char 'λ' <|> char '\\')
    >> currying <$> identifier <*> many (whiteSpace *> identifier)
    <*> (whiteSpace *> string "->" *> expr)

term :: Parser (ExprP Literal)
term = whiteSpace *> term' <* whiteSpace
    where
        term' = try (parens expr)
            <|> section
            <|> lambda
            -- <|> Halt <$> (char '#' *> term)
            <|> External <$> literal
            <|> Var <$> identifier
            
section :: Parser (ExprP Literal)
section = Var <$> (char '(' *> operator <* char ')')

expr :: Parser (ExprP Literal)
expr = foldl Apply <$> term <*> many term


literal :: Parser Literal
literal = LNat <$> natural
    <|> LStr <$> stringLiteral
    <|> LChar <$> charLiteral
