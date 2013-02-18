{-# LANGUAGE ImplicitParams #-}
module Language.LazyZ.Syntax where

import Control.Applicative (pure, some, (<$>), (*>), (<*>), (<*), (<$))
import Data.Char (isSpace, ord)
import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Data.Void
import Data.Combinator.Encoding
import Language.LazyZ.Program (ExprP(..))
import Language.LazyK.Syntax (ccParser)
import Data.Combinator.Expr (Expr(..))
import Control.Monad.Identity

type OpTable = OperatorTable String () Identity (ExprP Literal)

data Literal = LNat Integer | LChar Char | LStr String deriving (Show, Eq)

encodeLiteral :: Literal -> Expr String
encodeLiteral (LNat n) = Extern $ "AI" ++ replicate (div (fromEnum n) 10) 'E' ++ replicate (mod (fromEnum n) 10 + 1) '!'
encodeLiteral (LChar c) = vacuous $ encode (Church $ toInteger $ ord c)

lexer = T.makeTokenParser haskellStyle

identifier = T.identifier lexer <|> parens operator
natural = T.natural lexer
stringLiteral = T.stringLiteral lexer
charLiteral = T.charLiteral lexer
parens = T.parens lexer
whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer

indentLevel = length . takeWhile isSpace

parseDecs :: (?optable :: OpTable) => Parser [(String, ExprP Literal)]
parseDecs = some definition

parseOpTable :: Parser OpTable
parseOpTable = map (map snd)
    <$> groupBy ((==) `on` fst)
    <$> sortBy (compare `on` fst)
    <$> concat
    <$> many (spaces *> def <* char ';') where
    def = pure <$> (try (infixp "--#infixl" AssocLeft) <|> try (infixp "--#infixr" AssocRight) <|> infixp "--#infix" AssocNone)
        <|> (many (satisfy (/=';')) *> pure [])
    infixp s a = lexeme $ do
        string s
        whiteSpace
        p <- natural
        whiteSpace
        op <- operator
        return (p, Infix ((\x y -> Var op `Apply` x `Apply` y) <$ reservedOp op) a)

definition :: (?optable :: OpTable) => Parser (String, ExprP Literal)
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

construct :: (?optable :: OpTable) => [String] -> ExprP e -> ExprP e
construct (x:xs) e = currying x xs e
construct [] e = e

currying p [] e = Lambda p e
currying p (p':ps) e = Lambda p (currying p' ps e)

lambda :: (?optable :: OpTable) => Parser (ExprP Literal)
lambda = (char 'λ' <|> char '\\')
    >> currying <$> identifier <*> many (whiteSpace *> identifier)
    <*> (whiteSpace *> string "->" *> expr)

quote :: (?optable :: OpTable) => Parser (ExprP a)
quote = string "[|" *> (vacuous <$> Embed <$> (ccParser :: Parser (Expr Void))) <* string "|]"

term :: (?optable :: OpTable) => Parser (ExprP Literal)
term = whiteSpace *> term' <* whiteSpace
    where
        term' = try (parens expr)
            <|> section
            <|> lambda
            <|> quote
            <|> External <$> literal
            <|> Var <$> identifier

termA :: (?optable :: OpTable) => Parser (ExprP Literal)
termA = foldl Apply <$> term <*> many term        

section :: (?optable :: OpTable) => Parser (ExprP Literal)
section = Var <$> (char '(' *> operator <* char ')')

expr :: (?optable :: OpTable) => Parser (ExprP Literal)
expr = buildExpressionParser ?optable termA where

literal :: Parser Literal
literal = LNat <$> natural
    <|> LStr <$> stringLiteral
    <|> LChar <$> charLiteral
