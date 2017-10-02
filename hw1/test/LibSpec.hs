module LibSpec where

import           Data.Foldable  (Foldable (..))
import           Data.List      (sort)
import           Data.Monoid    (Sum (..))
import           Data.Semigroup (Semigroup (..))
import           Lib
import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 (1, 2, 3) `shouldBe` (1, 2, 3)
        order3 (3, 2, 1) `shouldBe` (1, 2, 3)
        order3 (2, 1, 3) `shouldBe` (1, 2, 3)
        order3 (1, 2, 3) `shouldBe` (1, 2, 3)
        order3 (1, 3, 2) `shouldBe` (1, 2, 3)

    it "highestBitHard" $ do
        highestBitHard 15 `shouldBe` (8, 3)
        highestBitHard 16 `shouldBe` (16, 4)
        highestBitHard 17 `shouldBe` (16, 4)

    it "smartReplicate" $ do
        smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]

    it "contains" $ do
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]

    it "removeAt" $ do
        removeAt 1 [1,2,3] `shouldBe` [1,3]
        removeAt 10 [1,2,3] `shouldBe` [1,2,3]
        removeAt 3 [1..5] `shouldBe` [1,2,3,5]
        removeAt 2 "abc" `shouldBe` "ab"

    it "removeItHard" $ do
        removeAtHard 1 [1,2,3] `shouldBe` (Just 2, [1,3])
        removeAtHard 10 [1,2,3] `shouldBe` (Nothing, [1,2,3])

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8], [3,6])

    it "stringSum" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97

    it "stringSumHard" $ do
        stringSumHard "1 1" `shouldBe` 2
        stringSumHard "100\n\t-3" `shouldBe` 97
        stringSumHard "-1 +1" `shouldBe` 0
        -- stringSumHard "+-1" `shouldThrow` anyErrorCall -- needs IO :(

    it "mergeSort" $ do
        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]

    it "nextDay" $ do
        nextDay Sun `shouldBe` Mon
        nextDay Mon `shouldBe` Tue

    it "afterDays" $ do
        afterDays Mon 0 `shouldBe` Mon
        afterDays Mon 1 `shouldBe` Tue
        afterDays Mon 7 `shouldBe` Mon

    it "isWeekend" $ do
        isWeekend Fri `shouldBe` False
        isWeekend Sun `shouldBe` True

    it "daysToParty" $ do
        daysToParty Fri `shouldBe` 0
        daysToParty Thu `shouldBe` 1
        daysToParty Sun `shouldBe` 5

    it "battle" $ do
        battleHard (Knight 1 3) (Monster 1 2) `shouldBe` (True, 3)
        battleHard (Monster 1 3) (Monster 1 2) `shouldBe` (True, 3)
        battleHard (Knight 1 3) (Knight 1 2) `shouldBe` (True, 3)
        battleHard (Knight 2 3) (Knight 1 2) `shouldBe` (True, 1)
        battleHard (Knight 1 5) (Knight 2 5) `shouldBe` (False, 6)

    it "vector len" $ do
        len (Vector2D 3 4) `shouldBe` 5

    it "vector sum" $ do
        sumVec (Vector2D 1 2) (Vector2D 4 2) `shouldBe` (Vector2D 5 4)
        sumVec (Vector3D 1 2 1) (Vector2D 4 2) `shouldBe` (Vector3D 5 4 1)
        sumVec (Vector3D 1 2 1) (Vector3D 4 2 1) `shouldBe` (Vector3D 5 4 2)

    it "dot mul" $ do
        dotMul (Vector2D 1 2) (Vector2D 4 2) `shouldBe` sqrt 8
        dotMul (Vector3D 1 2 1) (Vector2D 4 2) `shouldBe` sqrt 8
        dotMul (Vector3D 1 2 1) (Vector3D 4 2 1) `shouldBe` 3

    it "distance" $ do
        distance (Vector2D 1 1) (Vector2D 1 1) `shouldBe` 0

    it "nats" $ do
        let two = S (S Z)
        2 `shouldBe` two
        natToInteger (2 + 2) `shouldBe` 4

    it "trees" $ do
        let li = [1,10,2,20,128,42,3]
        size (fromList li) `shouldBe` 7
        toList (fromList li) `shouldBe` sort li

    it "splitJoin" $ do
        splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
        joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"

    it "maybeConcat" $ do
        maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]

    it "Name" $ do
        (Name "root" <> Name "server") `shouldBe` Name "root.server"

    it "monoTrees" $ do
        let li1 = [91,25,32,83,34,53,60,84,17,96]
        let li2 = [88,56,38,68,62,27,76,74]
        (toList $ (fromList li1) `mappend` (fromList li2)) `shouldBe` sort (li1 ++ li2)

