module Part1Spec where

import           Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict      as Map
import           Part1
import           Test.Hspec



spec :: Spec
spec = do
    it "sanity" $ do
        someFunc `shouldBe` "someFunc"
    let map1 = Map.singleton "x" 1
    let map0 = Map.empty
    it "literal" $ do
        runReaderT (eval (Lit 1)) map1 `shouldBe` Right 1
    it "variable" $ do
        runReaderT (eval (Var "x")) map1 `shouldBe` Right 1
        runReaderT (eval (Var "x")) map0 `shouldBe` Left (MissingVariable "x")
        runReaderT (eval (Var "y")) map1 `shouldBe` Left (MissingVariable "y")
    it "add" $ do
        runReaderT (eval (Var "x" `Add` Lit 0)) map1 `shouldBe` Right 1
        runReaderT (eval (Lit 2 `Add` Lit 2)) map0 `shouldBe` Right 4
    it "div" $ do
        runReaderT (eval (Var "x" `Div` Lit 0)) map1 `shouldBe` Left ZeroDivision
        runReaderT (eval (Lit 5 `Div` Lit 2)) map0 `shouldBe` Right 2
    it "let" $ do
        runReaderT (eval ("x" `Let` (Lit 2) $ Var "x")) map0 `shouldBe` Right 2
        runReaderT (eval ("y" `Let` (Lit 2) $ Var "x" `Add` Var "y")) map1 `shouldBe` Right 3
        runReaderT (eval (Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x")))) map1 `shouldBe` Right 7
        runReaderT (eval (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x")))) map1 `shouldBe` Left (MissingVariable "y")
        runReaderT (eval (Let ("y") (Lit 2 `Add` Lit 2) (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))))) map0 `shouldBe` Right 10
        runReaderT (eval ((Let ("y") (Lit 2) (Var "y")) `Add` (Let ("x") (Lit 2) (Var "x" `Add` Var "y")))) map0 `shouldBe` Left (MissingVariable "y")



