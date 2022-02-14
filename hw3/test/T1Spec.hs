{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T1Spec
  ( spec
  ) where

import HW3.Base
import SpecS
import Text.RawString.QQ
import qualified Hedgehog.Gen as Gen
import Test.Hspec.Hedgehog (hedgehog, forAll, (===))

spec :: Spec
spec = do
  describe "numbers" $ do
    it "simple" $ do
      testEval "2.0" `shouldBe` Ok "2"
      testEval "2e4" `shouldBe` Ok "20000"
      testEval "-3" `shouldBe` Ok "-3"
      testEval "-3.43" `shouldBe` Ok "-3.43"
    it "nums" $ do
      testEval "add(1, 3)" `shouldBe` Ok "4"
      testEval "sub(54, 4)" `shouldBe` Ok "50"
      testEval "mul(11, 3)" `shouldBe` Ok "33"
      testEval "div(10, 2)" `shouldBe` Ok "5"
      testEval "add(500, 12)" `shouldBe` Ok "512"
      testEval "sub(10, 100)" `shouldBe` Ok "-90"
      testEval "div(57, 190)" `shouldBe` Ok "0.3"
      testEval "mul(23, 768)" `shouldBe` Ok "17664"
      testEval "mul(2, 10)" `shouldBe` Ok "20"
      testEval  "sub(1000, 7)" `shouldBe` Ok "993"
      testEval  "div(3, 5)" `shouldBe` Ok "0.6"
      testEval  "add(div(2, 1), mul(11,6))" `shouldBe` Ok "68"
      testEval  "mul(div(3, 4), sub(11,1))" `shouldBe` Ok "7.5"
      testEval  "div(add(mul(2, 5), 1), sub(11,6))" `shouldBe` Ok "2.2"
  it "parsing" $ do
    testEval "-8.15" `shouldBe` Ok "-8.15"
    testEval "div(-1, 7)" `shouldBe` Ok "-1/7"
    testEval "div(16, 3)" `shouldBe` Ok "5 + 1/3"
    testEval "-10 - div(1, 7)" `shouldBe` Ok "-10 - 1/7"
  describe "funcs" $ do
    it "funcs" $ do
      testEval "add" `shouldBe` Ok "add"
      testEval "sub" `shouldBe` Ok "sub"
      testEval "div" `shouldBe` Ok "div"
      testEval "mul" `shouldBe` Ok "mul"
  describe "errors" $ do
    it "trivial errors" $ do
      testEval "mul(1)" `shouldBe` EvalError HiErrorArityMismatch
      testEval "add(10, 20, 30)" `shouldBe` EvalError HiErrorArityMismatch
      testEval "div(1, 0)" `shouldBe` EvalError HiErrorDivideByZero
      testEval "div(1, sub(5, 5))" `shouldBe` EvalError HiErrorDivideByZero
      testEval "sub(10, add)" `shouldBe` EvalError HiErrorInvalidArgument
      testEval "15(2)" `shouldBe` EvalError HiErrorInvalidFunction

