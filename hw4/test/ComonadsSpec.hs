{-# LANGUAGE OverloadedStrings #-}

module ComonadsSpec where

import           Comonads
import           Control.Comonad (extract, (=>>))
import           Test.Hspec

spec :: Spec
spec = do
    it "project builder" $ do
        (extract $ buildProject "cool-project") `shouldBe`
            (Project "cool-project" False False False)
        (extract $ buildProject "cool-project" =>> benchs) `shouldBe`
            (Project "cool-project" True False False)
        (extract $ buildProject "cool-project" =>> github) `shouldBe`
            (Project "cool-project" False True False)
        (extract $ buildProject "cool-project" =>> travis) `shouldBe`
            (Project "cool-project" False False False)
        (extract $ buildProject "cool-project" =>> github =>> travis) `shouldBe`
            (Project "cool-project" False True True)
        (extract $ buildProject "cool-project" =>> travis =>> github) `shouldBe`
            (Project "cool-project" False True True)
        (extract $ buildProject "cool-project" =>> travis =>> github =>> benchs) `shouldBe`
            (Project "cool-project" True True True)
