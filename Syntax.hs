module LazyZ.Syntax where
import Text.Parsec
import Control.Applicative (some, (<$>), (*>), (<*>), (<*))
import Text.Parsec.String
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as T

import Data.Char (isSpace, ord)

import LazyZ.Encoding (encodeNat', fromString', fromList)
import LazyZ.Program (ExprP(..))
import LazyZ.Expr (Expr)

data Literal = LNat Integer | LChar Char | LStr String deriving Show

encodeLiteral :: Literal -> Expr e
encodeLiteral (LNat n) = encodeNat' n
encodeLiteral (LStr x) = fromString' x
encodeLiteral (LChar c) = encodeNat' (toInteger $ ord c)

lexer = T.makeTokenParser haskellStyle

identifier = T.identifier lexer <|> parens operator
natural = T.natural lexer
stringLiteral = T.stringLiteral lexer
charLiteral = T.charLiteral lexer
parens = T.parens lexer
whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
commaSep = T.commaSep lexer
operator = T.operator lexer

indentLevel = length . takeWhile isSpace

lazyZparser = some definition

definition :: Parser (String, ExprP Literal)
definition = lexeme $ do
    whiteSpace
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
lambda = string "\\"
    >> currying <$> identifier <*> many (whiteSpace *> identifier)
    <*> (whiteSpace *> string "->" *> expr)

term :: Parser (ExprP Literal)
term = many (char ' ') *> term' <* many (char ' ')
    where
        term' = try (parens expr)
            <|> section
            <|> lambda
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
