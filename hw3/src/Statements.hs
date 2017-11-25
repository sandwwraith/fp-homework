module Statements where

import           Expressions         (Expr (..), ExprMap)

import           Control.Monad.State (StateT, get, gets, lift, put)
import qualified Data.Map.Strict     as Map


data VariableError = Redefinition String | Undefined String deriving (Show)

type MutExprCtx = StateT ExprMap (Either VariableError)

def :: String -> Int -> MutExprCtx ()
def name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> put $ Map.insert name value m
        Just _  -> lift $ Left (Redefinition name)

update :: String -> Int -> MutExprCtx ()
update name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> lift $ Left (Undefined name)
        Just _  -> put $ Map.insert name value m

getVar :: String -> MutExprCtx Int
getVar name = gets (Map.lookup name) >>= lift . maybe (Left (Undefined name)) Right

data Statement = Def String Expr | Assgmnt String Expr deriving (Show)


