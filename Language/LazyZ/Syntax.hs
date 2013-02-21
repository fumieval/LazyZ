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
import Language.LazyZ.Program (ExprP(..))
import Language.LazyK.Syntax (ccParser)
import Data.Combinator.Expr (Expr(..))
import Data.Combinator.Church
import Control.Monad.Identity
import qualified Codec.Binary.UTF8.String as UTF8

type OpTable = OperatorTable String () Identity (ExprP Void)

primNat :: Integer -> ExprP Void
primNat n = Apply (Var "prim_nat") $ 
    LiftExpr (encodeInteger n)

primList :: [ExprP e] -> ExprP e
primList xs = Apply (Var "prim_list") $ 
    Lambda "cons" $ Lambda "nil"
    $ foldr (\y ys -> Var "cons" `Apply` y `Apply` ys) (Var "nil") xs

literal :: (?optable :: OpTable) => Parser (ExprP Void)
literal = primNat <$> natural
    <|> primList <$> map (primNat.fromIntegral) <$> UTF8.encode <$> stringLiteral
    <|> primNat <$> toEnum <$> fromEnum <$> charLiteral
    <|> (symbol "[" *> (primList <$> expr `sepBy` symbol ",") <* symbol "]")

lexer = T.makeTokenParser haskellStyle

identifier = T.identifier lexer <|> parens operator
natural = T.natural lexer
stringLiteral = T.stringLiteral lexer
charLiteral = T.charLiteral lexer
parens = T.parens lexer
whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
symbol = T.symbol lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer

indentLevel = length . takeWhile isSpace

parseDecs :: OpTable -> Parser [(String, ExprP Void)]
parseDecs t = do
    let ?optable = t
    some definition

parseOpTable :: Parser OpTable
parseOpTable = map (map snd)
    <$> groupBy ((==) `on` fst)
    <$> sortBy (compare `on` (negate.fst))
    <$> concat
    <$> many (spaces *> def <* char ';') where
    def = pure <$> (try (infixp "--#infixl" AssocLeft)
            <|> try (infixp "--#infixr" AssocRight)
            <|> try (infixp "--#infix" AssocNone))
        <|> (many (satisfy (/=';')) *> pure [])
    infixp s a = lexeme $ do
        string s
        whiteSpace
        p <- natural
        whiteSpace
        op <- operator
        return (p, Infix ((\x y -> Var op `Apply` x `Apply` y) <$ reservedOp op) a)

definition :: (?optable :: OpTable) => Parser (String, ExprP Void)
definition = lexeme $ do
    whiteSpace
    name <- identifier
    args <- many (whiteSpace *> identifier)
    symbol "="
    definition <- expr
    whiteSpace
    char ';'
    return (name, construct args definition)

construct :: (?optable :: OpTable) => [String] -> ExprP e -> ExprP e
construct (x:xs) e = currying x xs e
construct [] e = e

currying p [] e = Lambda p e
currying p (p':ps) e = Lambda p (currying p' ps e)

lambda :: (?optable :: OpTable) => Parser (ExprP Void)
lambda = (char 'λ' <|> char '\\')
    >> currying <$> identifier <*> many (whiteSpace *> identifier)
    <*> (whiteSpace *> string "->" *> expr)

quote :: (?optable :: OpTable) => Parser (ExprP a)
quote = string "[|" *> (vacuous <$> LiftExpr <$> (ccParser :: Parser (Expr Void))) <* string "|]"

term :: (?optable :: OpTable) => Parser (ExprP Void)
term = whiteSpace *> term' <* whiteSpace where
    term' = try section
        <|> parens expr
        <|> lambda
        <|> try quote
        <|> literal
        <|> Var <$> identifier

termA :: (?optable :: OpTable) => Parser (ExprP Void)
termA = foldl Apply <$> term <*> many term

section :: (?optable :: OpTable) => Parser (ExprP Void)
section = Var <$> (char '(' *> operator <* char ')')

expr :: (?optable :: OpTable) => Parser (ExprP Void)
expr = buildExpressionParser ?optable termA where

