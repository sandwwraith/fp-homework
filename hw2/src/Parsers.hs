module Parsers where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (void, (>=>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace,
                                      isUpper)
import           Data.List           (foldl')
import qualified Data.Map.Strict     as Map

newtype Parser a
    = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

charP :: Char -> Parser Char
charP c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

mapFirst :: (a -> b) -> (a,c) -> (b,c)
mapFirst f (a, c) = ((f a), c)

instance Functor Parser where
  -- fmap f (Parser runner) = Parser $ runner >=> return . (mapFirst f)
  fmap f (Parser runner) = Parser $ \s -> (mapFirst f) <$> (runner s)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  (Parser p1) <*> (Parser p2) = Parser $
    p1 >=> \(f, s) ->
    p2 s >>= \(val, s2) -> return (f val, s2)


abParser :: Parser (Char, Char)
abParser = (,) <$> charP 'a' <*> charP 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = pack <$> posInt <* charP ' ' <*> posInt
  where
    pack :: Integer -> Integer -> [Integer]
    pack a b = [a, b]


instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

intOrUppercase :: Parser ()
intOrUppercase = void (satisfy isUpper) <|> void posInt

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident deriving Show

data SExpr = A Atom | Comb [SExpr] deriving Show

atomParser :: Parser Atom
atomParser = (N <$> posInt) <|> (I <$> ident)


parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseP <* spaces
  where
    parseP = A <$> atomParser <|> Comb <$> (charP '(' *> oneOrMore parseSExpr <* charP ')')


instance Monad Parser where
  return = pure

  (Parser p) >>= f = Parser $ p >=> \(val, s) -> runParser (f val) s

letParser :: Parser String
letParser = ident >>= \i -> if (i == "let") then return i else empty

data LExpr = LExpr Ident [Atom] deriving Show

exprParser :: Parser LExpr
exprParser = LExpr <$> (letParser *> spaces *> ident) <* spaces <* charP '=' <* spaces <*> linearParser

linearParser :: Parser [Atom]
linearParser = (:) <$> atomParser <*> zeroOrMore (spaces *> charP '+' *> spaces *> atomParser <* spaces)

letExprParser :: Parser [LExpr]
letExprParser = oneOrMore (exprParser <* (spaces <|> ((:[]) <$> (charP '\n'))))

test :: String
test = unlines [
    "let x = 1 + 2 + 5",
    "let   y = x+x",
    "let z=0+    x   + y + 8 "
    ]

type MMap = Map.Map Ident Integer

optimize :: [LExpr] -> [LExpr]
optimize [] = []
optimize li = fst $ foldl' go ([], Map.empty) li
  where
    go :: ([LExpr], MMap) -> LExpr -> ([LExpr], MMap)
    go (xs, oldmap) x = let (newx, newmap) = runOptimize oldmap x in (xs ++ [newx], newmap)
      where
        runOptimize :: MMap -> LExpr -> (LExpr, MMap)
        runOptimize mmap (LExpr name atoms) = let sk = foldr value 0 atoms in (LExpr name [N sk], Map.insert name sk mmap)
          where
            value :: Atom -> Integer -> Integer
            value (N a) b = a + b
            value (I n) b = (mmap Map.! n) + b

