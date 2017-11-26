{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Statements where

import           Expressions         (Expr (..), ExprMap, doEval)

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.State (MonadState, StateT, get, put, runStateT)
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

data Statement
    = Def { varName    :: String
          , expression :: Expr }
    | Assgmnt { varName    :: String
              , expression :: Expr }
    deriving (Eq, Show)

compute :: (MonadState ExprMap m, MonadThrow m) => [Statement] -> m ExprMap
compute [] = get
compute (x:xs) = do
    vars <- get
    c <- doEval (expression x) vars
    case x of
        Def name _     -> def name c
        Assgmnt name _ -> update name c
    compute xs

doCompute :: (MonadThrow m) => [Statement] -> m ExprMap
doCompute stmt = fst <$> runStateT (compute stmt) Map.empty
