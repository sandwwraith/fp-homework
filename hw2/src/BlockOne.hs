{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module BlockOne where

import qualified Control.Category (Category (id, (.)))
import           Control.Monad    ((>=>))
import           Data.Maybe       (fromMaybe)

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
    binOp (Const a) _ _ = Right a
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

instance Control.Category.Category (~>) where
  id = total Prelude.id
  f . g = Partial (apply g >=> apply f)

  {- 1. (.) f id === f
        Partial (apply id >=> apply f) === f                 (definition of .)
        Partial (apply (total Prelude.id) >=> apply f) === f (definition of id)
        Partial (\x -> apply (total Prelude.id) x >>= apply f) === f         (definition of >=>)
        Partial (\x -> Just x >>= apply f) === f             (applying total function)
        Partial (\x -> apply f x) === f                      (bind instance for Maybe)
        apply (Partial (\x -> apply f x)) === apply f
        \x -> apply f x === apply f                          (definition of apply for Partial)
        apply f === apply f                                  (eta-reduction)

     2. (.) id f === f
        Partial (apply f >=> apply id) === f                        (definition of .)
        Partial (apply f >=> apply (total Prelude.id)) === f        (definition of id)
        Partial (\x -> apply f x >>= apply (total Prelude.id)) === f  (definition of >=>)
        Partial (\x -> apply f x >>= Just) === f                    (applying total function)
        Partial (\x -> apply f x) === f                             (Just is return, m >>= return  =  m)
        apply $ Partial (\x -> apply f x) === apply f
        \x -> apply f x === apply f                                 (definition of apply for Partial)
        apply f === apply f                                         (eta-reduction)

     3. (f . g) . h === f . (g . h)
        Left part:
        Partial (apply h >=> apply (Partial (apply g >=> apply f)))
        Partial (\x -> apply h x >>= apply (Partial (apply g >=> apply f))) (definition of >=>)
        Partial (\x -> apply h x >>= (apply g >=> apply f))                 (applying Partial)
        Partial (\x -> apply h x >>= (\y -> apply g y >>= apply f))         (definition of >=>)
        Partial (\x -> (apply h x >>= apply g) >>= apply f)                 (Monad law: m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h)

        Right part:
        Partial (apply (Partial (apply h >=> apply g)) >=> apply f)
        Partial ((apply h >=> apply g) >=> apply f)
        Partial ((\y -> apply h y >>= apply g) >=> apply f)
        Partial (\x -> (\y -> apply h y >>= apply g) x >>= apply f)
        Partial (\x -> apply h x >>= apply g >>= apply f)

        Partial (\x -> (apply h x >>= apply g) >>= apply f) === Partial (\x -> apply h x >>= apply g >>= apply f)
  -}


bin :: Int -> [[Int]]
bin x
 | x < 0 = error "Illegal argument"
 | x == 0 = [[]]
 | otherwise = bin (x - 1) >>= \li -> [0 : li, 1 : li]

combinations :: Int -> Int -> [[Int]]
combinations n k
  | n < 1 || k > n || k < 1 = error "Illegal argument"
  | otherwise = go k [1..n]
  where
    go :: Int -> [Int] -> [[Int]]
    go 0 _       = [[]]
    go _ []      = []
    go k_ (x:xs) = (go (k_ - 1) xs >>= \li -> [x:li]) ++ go k_ xs

permutations :: [a] -> [[a]]
permutations []  = [[]]
permutations [a] = [[a]]
permutations (y:ys) = permutations ys >>= \li -> insertEvery y li
  where
    insertEvery x []        = [[x]]
    insertEvery x li@(l:ls) = (x:li):(insertEvery x ls >>= \ll -> [l:ll])
