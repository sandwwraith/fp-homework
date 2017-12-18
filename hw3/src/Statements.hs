{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Statements where

import           Expressions         (Expr (..), ExprMap, ExpressionError,
                                      doEval)

import           Control.Monad       (replicateM_, void)
import           Control.Monad.Catch (Exception, MonadCatch, MonadThrow, catch,
                                      throwM)
import           Control.Monad.State (MonadIO, MonadState, StateT, get, liftIO,
                                      modify, put, runStateT)
import qualified Data.Map.Strict     as Map
import           Data.Typeable       (Typeable)

data StatementError
    = Redefinition String
    | Undefined String
    | EvaluationError Statement ExpressionError
    deriving (Eq, Typeable)

instance Show StatementError where
    show (Redefinition s) = "Variable " ++ s ++ " already defined"
    show (Undefined s) = "Variable " ++ s ++ " was not defined"
    show (EvaluationError stmt e) = "Error \"" ++ show e ++ "\" occured during evaluation of statement " ++ show stmt

instance Exception StatementError

type MutExprCtx = StateT ExprMap (Either StatementError)

def :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
def name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> put $ Map.insert name value m
        Just _  -> throwM (Redefinition name)

update :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
update name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> throwM (Undefined name)
        Just _  -> put $ Map.insert name value m

overwrite :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
overwrite name value = modify $ Map.insert name value

data Statement
    = Def { varName    :: String
          , expression :: Expr }
    | Assgmnt { varName    :: String
              , expression :: Expr }
    | PrintVal { expression :: Expr }
    | ReadVal { varName :: String }
    | ForLoop { expression :: Expr
              , bound      :: Expr
              , body       :: [Statement] }
    deriving (Eq, Show)

compute ::
       (MonadState ExprMap m, MonadCatch m, MonadIO m)
    => [Statement]
    -> m ExprMap
compute [] = get
compute (x:xs) = do
    vars <- get
    c <-
        case x of
            ReadVal _ -> read <$> liftIO getLine
            _         -> evalExpr x (expression x) vars
    case x of
        Def name _     -> def name c
        Assgmnt name _ -> update name c
        PrintVal _     -> liftIO $ print c
        ReadVal name   -> overwrite name c
        ForLoop _ boundExpr stmts -> do
            to <- evalExpr x boundExpr vars
            replicateM_ (to - c) (compute stmts)
    compute xs
      where
        evalExpr stmt expr varz = catch (doEval expr varz) (\e -> throwM $ EvaluationError stmt e)

newtype StatementCtx a = StatementCtx { runStatement :: StateT ExprMap (IO) a}
            deriving (Functor, Applicative, Monad, MonadIO,
                      MonadState ExprMap, MonadThrow, MonadCatch)

executeProgram :: StatementCtx a -> IO a
executeProgram ctx = fst <$> runStateT (runStatement ctx) Map.empty

interpret :: [Statement] -> IO ExprMap
interpret = executeProgram . compute

interpret_ :: [Statement] -> IO ()
interpret_ = void . executeProgram . compute
