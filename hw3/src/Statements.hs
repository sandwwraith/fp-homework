{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Statements where

import           Expressions         (Expr (..), ExprMap, doEval)

import           Control.Monad       (replicateM_, void)
import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.State (MonadIO, MonadState, StateT, get, liftIO,
                                      modify, put, runStateT)
import qualified Data.Map.Strict     as Map
import           Data.Typeable       (Typeable)

data StatementError
    = Redefinition String
    | Undefined String
    deriving (Eq, Show, Typeable)

instance Exception StatementError

type MutExprCtx = StateT ExprMap (Either StatementError)

def :: (MonadState ExprMap m, MonadThrow m) => String -> Int -> m ()
def name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> put $ Map.insert name value m
        Just _  -> throwM (Redefinition name)

update :: (MonadState ExprMap m, MonadThrow m) => String -> Int -> m ()
update name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> throwM (Undefined name)
        Just _  -> put $ Map.insert name value m

overwrite :: (MonadState ExprMap m, MonadThrow m) => String -> Int -> m ()
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
       (MonadState ExprMap m, MonadThrow m, MonadIO m)
    => [Statement]
    -> m ExprMap
compute [] = get
compute (x:xs) = do
    vars <- get
    c <-
        case x of
            ReadVal _ -> read <$> liftIO getLine
            _         -> doEval (expression x) vars
    case x of
        Def name _     -> def name c
        Assgmnt name _ -> update name c
        PrintVal _     -> liftIO $ print c
        ReadVal name   -> overwrite name c
        ForLoop _ boundExpr stmts -> do
            to <- doEval boundExpr vars
            replicateM_ (to - c) (compute stmts)
    compute xs

newtype StatementCtx a = StatementCtx { runStatement :: StateT ExprMap (IO) a}
            deriving (Functor, Applicative, Monad, MonadIO,
                      MonadState ExprMap, MonadThrow)

executeProgram :: StatementCtx a -> IO a
executeProgram ctx = fst <$> runStateT (runStatement ctx) Map.empty

interpret :: [Statement] -> IO ExprMap
interpret = executeProgram . compute

interpret_ :: [Statement] -> IO ()
interpret_ = void . executeProgram . compute
