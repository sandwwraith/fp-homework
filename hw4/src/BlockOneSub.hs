{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BlockOneSub where

import           BlockOne

data MyData = MyData
    { foo :: String
    , bar :: Int
    } deriving (Show)

genShowText ''MyData
