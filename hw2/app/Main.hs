module Main where

import           BlockOne
import           Control.Monad.State

main :: IO ()
main = putStrLn someFunc

-- Examples from lecture
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x : xs)

evals :: String -> Stack
evals s = execState (forM_ (words s) doPrefix) []

doPrefix :: String -> State Stack ()
doPrefix "*" = pop >>= \x -> pop >>= \y -> push (x * y)
doPrefix "+" = pop >>= \x -> pop >>= \y -> push (x + y)
doPrefix n   = push (read n)
