{-# LANGUAGE DuplicateRecordFields #-}

module Part1 where

import           Control.Monad.Reader (ReaderT, asks, lift, liftM2, local)
import qualified Data.Map.Strict      as Map

someFunc :: String
someFunc = "someFunc"

data Expr
    = Var String
    | Lit Int
    | Add { op1 :: Expr
          , op2 :: Expr }
    | Sub { op1 :: Expr
          , op2 :: Expr }
    | Mul { op1 :: Expr
          , op2 :: Expr }
    | Div { op1 :: Expr
          , op2 :: Expr }
    | Let String Expr Expr
    deriving (Show)


data ExpressionError = MissingVariable String | ZeroDivision deriving (Eq)
instance Show ExpressionError where
    show (MissingVariable x) = "Variable " ++ x ++ " is missing"
    show (ZeroDivision)      = "Division by zero"

type ExprMap = Map.Map String Int
type ExprCtx = ReaderT ExprMap (Either ExpressionError) Int

eval :: Expr -> ExprCtx
eval (Lit n) = return n
eval (Var x) = asks (Map.lookup x) >>= lift . maybe (Left (MissingVariable x)) Right
eval (Let name value expr) = eval value >>= \val -> local (Map.insert name val) (eval expr)

eval (Add a b) = liftM2 (+) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Div a b) = do
    a1 <- eval a
    b1 <- eval b
    lift $ case b1 of
        0 -> Left ZeroDivision
        _ -> Right $ a1 `div` b1
