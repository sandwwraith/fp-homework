module ExpressionsSpec where

import           Expressions

import qualified Data.Map.Strict as Map
import           Test.Hspec

spec :: Spec
spec = do
    let map1 = Map.singleton "x" 1
    let map0 = Map.empty
    it "literal" $ do
        doEval (Lit 1) map1 `shouldReturn` 1
    it "variable" $ do
        doEval (Var "x") map1 `shouldReturn` 1
        doEval (Var "x") map0 `shouldThrow` (== MissingVariable "x")
        doEval (Var "y") map1 `shouldThrow` (== MissingVariable "y")
    it "add" $ do
        doEval (Var "x" `Add` Lit 0) map1 `shouldReturn` 1
        doEval (Lit 2 `Add` Lit 2) map0 `shouldReturn` 4
    it "div" $ do
        doEval (Var "x" `Div` Lit 0) map1 `shouldThrow` (== ZeroDivision)
        doEval (Lit 5 `Div` Lit 2) map0 `shouldReturn` 2
    it "let" $ do
        doEval ("x" `Let` (Lit 2) $ Var "x") map0 `shouldReturn` 2
        doEval ("y" `Let` (Lit 2) $ Var "x" `Add` Var "y") map1 `shouldReturn` 3
        doEval (Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))) map1 `shouldReturn`
            7
        doEval (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))) map1 `shouldThrow`
            (== MissingVariable "y")
        doEval
            (Let
                 ("y")
                 (Lit 2 `Add` Lit 2)
                 (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))))
            map0 `shouldReturn`
            10
        doEval
            ((Let ("y") (Lit 2) (Var "y")) `Add`
             (Let ("x") (Lit 2) (Var "x" `Add` Var "y")))
            map0 `shouldThrow`
            (== MissingVariable "y")
