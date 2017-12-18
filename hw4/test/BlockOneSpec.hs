module BlockOneSpec where

import           BlockOne
import           Test.Hspec



spec :: Spec
spec = do
  it "sanity" $ do
    2 + 2 `shouldBe` 4
