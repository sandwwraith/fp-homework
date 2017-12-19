{-# LANGUAGE TemplateHaskell #-}

module BlockOneSpec where

import           BlockOne
import           Test.Hspec



spec :: Spec
spec = do
  it "sanity" $ do
    2 + 2 `shouldBe` 4
  it "indices" $ do
    $(choseByIndices 4 [2, 0]) ("hello", 10, [4,3], 2) `shouldBe` ([4, 3], "hello")
    $(choseByIndices 4 [1, 1, 3, 1, 1]) ("hello", 10, [4,3], 2) `shouldBe` (10, 10, 2, 10, 10)
