module Main where

import           Data.ByteString.UTF8 (fromString)
import           Parser               (parseAndEval)

someFunc :: String
someFunc = "someFunc"

main :: IO ()
main = (fromString <$> getLine) >>= parseAndEval >>= print
