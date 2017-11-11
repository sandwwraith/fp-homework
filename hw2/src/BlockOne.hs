{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module BlockOne where

import           Data.Maybe (fromMaybe)

someFunc :: String
someFunc = "someFunc"

data Expr
  = Const Int
  | Add { op1 :: Expr
        , op2 :: Expr }
  | Sub { op1 :: Expr
        , op2 :: Expr }
  | Mul { op1 :: Expr
        , op2 :: Expr }
  | Div { op1 :: Expr
        , op2 :: Expr }
  | Pow { op1 :: Expr
        , op2 :: Expr }

data ArithmeticError
  = ZeroDivision
  | ZeroPowered
  deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval expr = eval (op1 expr) >>= \x -> eval (op2 expr) >>= \y -> (binOp expr x y)
  where
    binOp (Add _ _) x y = Right $ x + y
    binOp (Sub _ _) x y = Right $ x - y
    binOp (Mul _ _) x y = Right $ x * y
    binOp (Div _ _) x y =
      if y == 0
        then Left ZeroDivision
        else Right $ x `div` y
    binOp (Pow _ _) x y =
      if (x == 0 && y == 0)
        then Left ZeroPowered
        else Right $ x ^ y

data a ~> b
  = Partial (a -> Maybe b) -- a partial function
  | Defaulted (a ~> b) b -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial (\x -> Just $ f x)

(>->) :: (Maybe m) -> Maybe m -> Maybe m
(>->) (Just x) _ = Just x
(>->) _ x        = x

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f)     = f
apply (Defaulted f d) = \x -> (apply f x) >-> Just d

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f x def = fromMaybe def ((apply f) x)

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault = Defaulted

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f a =
  case (f `apply` a) of
    Nothing -> False
    Just _  -> True

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f1 f2 = Partial (\x -> (apply f1 x) >-> (apply f2 x))

bin :: Int -> [[Int]]
bin 0 = [[]]
bin x = bin (x - 1) >>= \li -> [0 : li, 1 : li]

combinations :: Int -> Int -> [[Int]]
combinations = undefined

permutations :: [a] -> [[a]]
permutations []  = [[]]
permutations [a] = [[a]]
permutations _   = undefined
