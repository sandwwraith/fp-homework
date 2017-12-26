module Main where

import           Criterion.Main
import qualified Data.List      as DL
import qualified Data.Set       as Set

nubStd :: Eq a => [a] -> [a]
nubStd = DL.nub

nubSorted :: (Eq a, Ord a) => [a] -> [a]
nubSorted arr = reverse $ distinct (DL.sort arr) []
  where
    distinct [] acc = acc
    distinct (x:xs) [] = distinct xs [x]
    distinct (x:xs) acc@(y:_) =
        if (x == y)
            then distinct xs acc
            else distinct xs (x : acc)

nubSet :: (Eq a, Ord a) => [a] -> [a]
nubSet = Set.toList . Set.fromList

allDist, allDistShuffled, repeatShuffled, repeatedSorted :: [Int]
allDist = [1..100]
allDistShuffled = [31, 60, 10, 35, 47, 58, 95, 43, 87, 26, 16, 45, 30, 65, 66, 41, 19, 62, 84, 36, 50, 46, 70, 89, 83, 34, 42, 80, 51, 74, 29, 14, 72, 27, 22, 90, 75, 21, 53, 49, 4, 37, 1, 9, 96, 23, 71, 82, 54, 52, 33, 20, 100, 61, 99, 63, 56, 98, 12, 13, 25, 7, 59, 78, 32, 55, 85, 94, 73, 40, 97, 3, 5, 18, 64, 11, 86, 15, 69, 93, 39, 79, 28, 81, 91, 88, 24, 68, 8, 2, 44, 67, 48, 57, 17, 6, 92, 77, 76, 38]
repeatShuffled = [6, 48, 30, 39, 24, 19, 43, 9, 42, 41, 11, 24, 21, 15, 30, 22, 35, 13, 36, 23, 15, 14, 4, 50, 17, 28, 8, 48, 41, 33, 10, 9, 2, 7, 23, 37, 3, 6, 28, 7, 25, 45, 8, 11, 29, 46, 12, 13, 49, 36, 26, 18, 37, 22, 47, 19, 34, 25, 1, 40, 16, 46, 45, 50, 16, 21, 20, 47, 31, 43, 32, 29, 31, 17, 35, 5, 18, 5, 38, 14, 2, 38, 26, 10, 32, 44, 44, 34, 39, 4, 40, 27, 27, 1, 12, 20, 33, 42, 3, 49]
repeatedSorted = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 50]



main :: IO ()
main =
    defaultMain
        [ bgroup "standard"
            [ bench "distinct" $ nf nubStd allDist
            , bench "distinctShuffled" $ nf nubStd allDistShuffled
            , bench "repeatedShuffled" $ nf nubStd repeatShuffled
            , bench "repeatedSorted" $ nf nubStd repeatedSorted
            ]
        , bgroup "sorted"
            [ bench "distinct" $ nf nubSorted allDist
            , bench "distinctShuffled" $ nf nubSorted allDistShuffled
            , bench "repeatedShuffled" $ nf nubSorted repeatShuffled
            , bench "repeatedSorted" $ nf nubSorted repeatedSorted
            ]
        , bgroup "set"
            [ bench "distinct" $ nf nubSet allDist
            , bench "distinctShuffled" $ nf nubSet allDistShuffled
            , bench "repeatedShuffled" $ nf nubSet repeatShuffled
            , bench "repeatedSorted" $ nf nubSet repeatedSorted
            ]
        ]
