{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Expressions                (Expr (..), ExprMap, doEval)
import           Statements                 (Statement (..), compute, interpret,
                                             interpret_)

import           Control.Applicative        (empty)
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)
import           Control.Monad.State        (MonadIO, runStateT)
import qualified Data.Map.Strict            as Map
import           Data.Typeable              (Typeable)
import           Data.Void                  (Void)

import qualified Data.ByteString            as PackedStr
import qualified Data.ByteString.Internal   as BS (c2w)
import qualified Data.ByteString.UTF8       as S8

import           Text.Megaparsec
import           Text.Megaparsec.Byte       (alphaNumChar, char, eol,
                                             letterChar, string)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr

type Str = S8.ByteString

type Parser = Parsec Void Str

data ParsingException = ParsingException (ParseError (Token Str) Void) deriving (Typeable)
instance Show ParsingException where
    show (ParsingException e) = parseErrorPretty e
instance Exception ParsingException

space1 :: Parser ()
space1 = skipSome (char (BS.c2w ' ') <|> char (BS.c2w '\t'))

indent :: Parser ()
indent = skipMany (char (BS.c2w ' ') <|> char (BS.c2w '\t'))

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
    rws :: [Str] -- list of hard keywords
    rws = ["let", "mut", "for"]


termParser :: Parser Expr
termParser = (Let <$> (symbol "(" *> rword "let" *> (S8.toString <$> identifier) <* symbol "=")
            <*> (exprParser <* symbol "in" ) <*> (exprParser <* symbol ")"))
    <|> parens exprParser
    <|> Var <$> (S8.toString <$> identifier)
    <|> Lit <$> integer

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

stmtParser :: Parser Statement
stmtParser = indent *> (
          Def <$> (rword "mut" *> (S8.toString <$> identifier) <* symbol "=") <*> exprParser
          <|> PrintVal <$> (symbol "<" *> exprParser)
          <|> ReadVal <$> (symbol ">" *> (S8.toString <$> identifier))
          <|> do
                _ <- rword "for" *> symbol "("
                ini <- exprParser
                _ <- rword "to"
                bnd <- exprParser
                _ <- symbol ")" *> symbol "{" *> eol
                inner <- programParser
                _ <- symbol "}"
                return $ ForLoop ini bnd inner
          <|> Assgmnt <$> ((S8.toString <$> identifier) <* symbol "=") <*> exprParser
        )

programParser :: Parser [Statement]
programParser = sc *> many (stmtParser <* eol)

useParser :: (MonadThrow m) => Parser a -> String -> Str -> m a
useParser p name input = either (throwM . ParsingException) return (parse p name input)

parseExprs :: (MonadThrow m) => Str -> m Expr
parseExprs = useParser exprParser ""

parseAndEval :: (MonadThrow m) => Str -> m Int
parseAndEval input = parseExprs input >>= flip doEval Map.empty

parseAndCompute :: (MonadIO m, MonadThrow m) => Str -> m ExprMap
parseAndCompute input = useParser stmtParser "" input >>= \stmt -> fst <$> runStateT (compute [stmt]) Map.empty

runProgram :: String -> Str -> IO ExprMap
runProgram name input = useParser programParser name input >>= interpret

runProgram_ :: String -> Str -> IO ()
runProgram_ name input = useParser programParser name input >>= interpret_
