{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Expressions
import           Parser
import           Statements

import qualified Data.ByteString.UTF8 as S8
import qualified Data.Map.Strict      as Map
import           Test.Hspec

spec :: Spec
spec = do
    it "2 + 2" $ do
        parseExprs "2 + 2" `shouldReturn` (Lit (2::Int)) `Add` (Lit 2)
        parseAndEval "2 + 2" `shouldReturn` 4
    it "x + 2" $ do
        parseAndEval "x + 2" `shouldThrow` (== MissingVariable "x")
    it "1+  (     let x  =2 in x   )  " $ do
        parseAndEval "1+  (     let x  =2 in x   )  " `shouldReturn` 3
    it "10 + 3 * (let x = 2 in x)" $ do
        parseAndEval "10 + 3 * (let x = 2 in x)" `shouldReturn` 16
    it "mut x = 2 + 2 * 2" $ do
        parseAndCompute "mut x = 2 + 2 * 2" `shouldReturn` Map.singleton "x" 6
    it "x = 2 + 2 * 2" $ do
        parseAndCompute "x = 2 + 2 * 2" `shouldThrow` (== Undefined "x")
    let testMut = S8.fromString $ unlines [
            "mut x = 3 + 4",
            "x = x * 5"
            ]
    it "sample program" $ do
        runProgram testMut `shouldReturn` Map.singleton "x" 35
