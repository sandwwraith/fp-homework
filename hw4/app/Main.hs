{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Lens        ((^?))
import           FSLens
import           System.Environment  (getArgs)

import           Control.Monad.State (MonadIO, MonadState, forever, get, liftIO,
                                      put, runStateT, when)

import           System.Exit         (exitSuccess)
import           System.IO           (isEOF)

type StateStack = [FS]

data WalkerState
    = WalkerState {
        history :: StateStack,
        dirCnt  :: Int,
        fileCnt :: Int
    }

collectStack :: StateStack -> String
collectStack st = collect st ""
  where
    collect :: StateStack -> String -> String
    collect [] s     = s
    collect (x:xs) s = (collect xs s) ++ "/" ++ (_name x)

instance Show WalkerState where
    show (WalkerState hist dc fc) = let cur = collectStack hist in
        unlines ["You in " ++ cur,
                "Files from root  " ++ cur ++ ": " ++ show fc,
                "Directories from " ++ cur ++ ": " ++ show dc
                ]

changeDir :: (MonadState WalkerState m, MonadIO m) => String -> m ()
changeDir newDir = do
    (WalkerState hist@(cur:_) dc fc) <- get
    let next = cur ^? cd newDir
    case next of
        Nothing -> liftIO $ putStrLn "No such dir"
        Just fs -> put $ (WalkerState (fs:hist) (dc + dirCount fs) (fc + fileCount fs))

goUp :: (MonadState WalkerState m, MonadIO m) => m ()
goUp = do
    (WalkerState (cur:prev) dc fc) <- get
    if (null prev)
        then liftIO $ putStrLn "Can't go up from root"
        else put $ (WalkerState prev (dc - dirCount cur) (fc - fileCount cur))

initial :: (MonadState WalkerState m, MonadIO m) => String -> m ()
initial root = do
    fs <- liftIO $ scanFS root
    let state = (WalkerState [fs] (dirCount fs) (fileCount fs))
    put $ state
    liftIO $ print state

loop :: (MonadState WalkerState m, MonadIO m) => m ()
loop = forever $ do
    done <- liftIO $ isEOF
    when done $ liftIO $ exitSuccess
    (cmd:path) <- words <$> (liftIO $ getLine)
    case cmd of
        "cd" -> changeDir (head path)
        "up" -> goUp
        _    -> liftIO $ putStrLn "Incorrect command"
    state <- get
    liftIO $ print state


main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            putStrLn "No input path, exiting"
        else do
            let root = head args
            _ <- runStateT (initial (root) *> loop) (WalkerState [] 0 0)
            print "OK"
