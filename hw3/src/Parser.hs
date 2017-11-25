{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Expressions                (Expr (..), ExpressionError (..),
                                             eval)
import           Statements                 (Statement (..), VariableError (..))

import           Control.Applicative        (empty)
import           Control.Monad.Reader       (runReaderT)
import           Data.Void

import qualified Data.ByteString            as PackedStr
import qualified Data.ByteString.UTF8       as S8
import qualified Data.Map.Strict            as Map

import           Text.Megaparsec
import           Text.Megaparsec.Byte       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr

type Str = S8.ByteString

type Parser = Parsec Void Str

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Str -> Parser Str
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: Str -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)


identifier :: Parser Str
identifier = (lexeme . try) (p >>= check)
  where
    p       = PackedStr.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
    rws :: [Str] -- list of reserved words
    rws = ["let", "in", "mut"]

exprParser :: Parser Expr
exprParser = makeExprParser termParser operators
  where
    operators :: [[Operator Parser Expr]]
    operators =
        [ [
            InfixL (Mul <$ symbol "*")
            , InfixL (Div <$ symbol "/")
        ]
        , [
            InfixL (Add <$ symbol "+")
            , InfixL (Sub <$ symbol "-")
        ]
        ]

termParser :: Parser Expr
termParser = (Let <$> (symbol "(" *> rword "let" *> (S8.toString <$> identifier) <* symbol "=")
            <*> (termParser <* symbol "in" ) <*> (termParser <* symbol ")"))
    <|> parens termParser
    <|> Var <$> (S8.toString <$> identifier)
    <|> Lit <$> integer


stmtParser :: Parser Statement
stmtParser = Def <$> (rword "mut" *> (S8.toString <$> identifier) <* symbol "=") <*> exprParser
          <|> Assgmnt <$> ((S8.toString <$> identifier) <* symbol "=") <*> exprParser


test :: Either (ParseError (Token Str) Void) (Either ExpressionError Int)
test = parse exprParser "hz" "x + 3 * (let x = 2 in x)" >>= \expr -> return $ runReaderT (eval expr) (Map.singleton "x" 1)
