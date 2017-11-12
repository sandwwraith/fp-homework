module BlockOneSpec where

import           BlockOne
import           Test.Hspec



spec :: Spec
spec = do
  it "sanity" $ do
    2 + 2 `shouldBe` 4
  it "expression" $ do
    eval (Const 4) `shouldBe` Right 4
    eval (Add (Const 2) (Const 2)) `shouldBe` Right 4
  it "partial" $ do
    let f = Partial (\x -> if even x then Just x else Nothing)
    isDefinedAt f 3 `shouldBe` False
    isDefinedAt f 2 `shouldBe` True
    apply f 3 `shouldBe` Nothing
    apply f 2 `shouldBe` Just 2
    applyOrElse f 3 1 `shouldBe` 1
    applyOrElse f 2 1 `shouldBe` 2
  it "defaulted" $ do
    let f = Partial (\x -> if even x then Just x else Nothing)
    let g = withDefault f 0
    isDefinedAt g 3 `shouldBe` True
    isDefinedAt g 2 `shouldBe` True
    apply g 3 `shouldBe` Just 0
    apply g 2 `shouldBe` Just 2
    applyOrElse g 3 1 `shouldBe` 0
    applyOrElse g 2 1 `shouldBe` 2
    let h = orElse f g
    apply h 5 `shouldBe` Just 0
    apply h 6 `shouldBe` Just 6
  it "bin" $ do
    bin 1 `shouldMatchList` [[1], [0]]
    bin 2 `shouldMatchList` [[1, 0], [0, 1], [1, 1], [0, 0]]
  -- it "permutations" $ do
  --   permutations [22, 10, 5] `shouldMatchList` [ [22, 10, 5]
  --                                               , [22, 5, 10]
  --                                               , [10, 22, 5]
  --                                               , [10, 5, 22]
  --                                               , [5, 22, 10]
  --                                               , [5, 10, 22]
  --                                               ]



