module BaseLensSpec where

import           BaseLens

import           Data.Function ((&))
import           Test.Hspec


spec :: Spec
spec = do
  it "view" $ do
    (1, 2)^._1 `shouldBe` 1
    (1, 2)^._2 `shouldBe` 2
  it "set" $ do
    ((1, 2) & _2 .~ 40) `shouldBe` (1, 40)
    ((1, 2) & _1 .~ 42) `shouldBe` (42, 2)
  it "over" $ do
    ((0,2) & _1 %~ (+ 1)) `shouldBe` (1,2)
    ((0,2) & _2 %~ (+ 1)) `shouldBe` (0,3)
