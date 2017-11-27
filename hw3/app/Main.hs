module Main where

import qualified Data.ByteString      as BS (readFile)
import           Data.ByteString.UTF8 (fromString)
import           Parser               (parseAndEval, runProgram_)
import           System.Environment   (getArgs)
import           System.IO            (hFlush, stdout)

someFunc :: String
someFunc = "someFunc"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            putStrLn "No program specified, running in interactive mode."
            putStr "Enter expression: "
            hFlush stdout
            expr <- fromString <$> getLine
            res <- parseAndEval expr
            putStrLn $ "Result: " ++ show res
        else do
            let name = (head args)
            program <- BS.readFile name
            runProgram_ name program
