{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expressions where

import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, liftM2,
                                       local, runReaderT)
import qualified Data.Map.Strict      as Map
import           Data.Typeable        (Typeable)

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
    | Let String
          Expr
          Expr
    deriving (Eq, Show)

data ExpressionError
    = MissingVariable String
    | ZeroDivision
    deriving (Eq, Typeable)

instance Show ExpressionError where
    show (MissingVariable x) = "Variable " ++ x ++ " is missing"
    show (ZeroDivision)      = "Division by zero"

instance Exception ExpressionError

type ExprMap = Map.Map String Int

eval :: (MonadReader ExprMap m, MonadThrow m) => Expr -> m Int
eval (Lit n) = return n
eval (Var x) = asks (Map.lookup x) >>= maybe (throwM (MissingVariable x)) return
eval (Let name value expr) =
    eval value >>= \val -> local (Map.insert name val) (eval expr)
eval (Add a b) = liftM2 (+) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Div a b) = do
    a1 <- eval a
    b1 <- eval b
    case b1 of
        0 -> throwM ZeroDivision
        _ -> return $ a1 `div` b1

doEval :: (MonadThrow m) => Expr -> ExprMap -> m Int
doEval expr vars = runReaderT (eval expr) vars
