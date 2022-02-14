{-# LANGUAGE QuasiQuotes #-}
module T5Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "infix" $ do
    it "easy list" $ do
      "list(1, 2, 3, 4, 5)" ~=?? Ok "[ 1, 2, 3, 4, 5 ]"
      "[]" ~=?? Ok "[ ]"
      "to-lower(\"Hello World\")" ~=?? Ok "\"hello world\""
      "reverse(\"sok\")" ~=?? Ok "\"kos\""
      "trim(\"kos\")" ~=?? Ok "\"kos\""
    it "list opers" $ do
      "[2, 4, 1] * 2" ~=?? Ok "[ 2, 4, 1, 2, 4, 1 ]"
      "[2, 4] * 0" ~=?? EvalError HiErrorInvalidArgument
      "list(1,2)" ~=?? Ok "[ 1, 2 ]"
      "range(5, 10.3)" ~=?? Ok "[ 5, 6, 7, 8, 9, 10 ]"
      "reverse(range(0.5, 70/8))" ~=?? Ok "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"
    it "fold" $ do
      "fold(sub, [2, 3, 4])" ~=?? Ok "-5"
      "fold(add, [2, 5] * 3)" ~=?? Ok "21"
      "fold(mul, range(1, 10))" ~=?? Ok "3628800"