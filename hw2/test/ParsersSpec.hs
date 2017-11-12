module ParsersSpec where

import           Data.Char  (isUpper)
import           Parsers
import           Test.Hspec



spec :: Spec
spec = do
  it "satisfy" $ do
    runParser (satisfy isUpper) "abc" `shouldBe` Nothing
    runParser (satisfy isUpper) "ABD" `shouldBe` Just ('A',"BD")
  it "charParser" $ do
    runParser (charP 'x') "xyz" `shouldBe` Just ('x',"yz")
  it "abParser" $ do
    runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")
    runParser abParser "accdef" `shouldBe` Nothing
  it "abParser_" $ do
    runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")
    runParser abParser_ "aebcdf" `shouldBe` Nothing
  it "intPair" $ do
    runParser intPair "12 34" `shouldBe` Just ([12,34],"")
    runParser intPair "1234" `shouldBe` Nothing
  it "intOrUppercase" $ do
    runParser intOrUppercase "342abcd" `shouldBe` Just ((),"abcd")
    runParser intOrUppercase "XYZ" `shouldBe` Just ((),"YZ")
    runParser intOrUppercase "foo" `shouldBe` Nothing
  it "zeroOrMore" $ do
    runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")
  it "oneOrMore" $ do
    runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
  it "spaces" $ do
    runParser spaces "   s" `shouldBe` Just ("   ","s")
    runParser spaces "s   s" `shouldBe` Just ("","s   s")
  it "ident" $ do
    runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
    runParser ident "foo33fA" `shouldBe` Just ("foo33fA","")
    runParser ident "2bad" `shouldBe` Nothing
    runParser ident "" `shouldBe` Nothing

