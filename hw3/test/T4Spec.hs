{-# LANGUAGE QuasiQuotes #-}
module T4Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "infix" $ do
    it "easy" $ do
      "length(\"Hello World\")" ~=?? Ok "11"
      "to-upper(\"Hello World\")" ~=?? Ok "\"HELLO WORLD\""
      "to-lower(\"Hello World\")" ~=?? Ok "\"hello world\""
      "reverse(\"sok\")" ~=?? Ok "\"kos\""
      "trim(\"kos\")" ~=?? Ok "\"kos\""
    it "string opers" $ do
       "\"/home/user\" / \"hi\"" ~=?? Ok "\"/home/user/hi\""
       "\"str\" * 3" ~=?? Ok "\"strstrstr\""
       "\"Good\" + \"oper\"" ~=?? Ok "\"Goodoper\""
    it "lookup" $ do
      "\"Hello World\"(0)" ~=?? Ok "\"H\""
      "\"Hello World\"(7)" ~=?? Ok "\"o\""
      "\"Hello World\"(432)" ~=?? Ok "null"
      "\"Hello World\"(-4343)" ~=?? Ok "null"
      "\"Hello World\"(-1)" ~=?? Ok "null"
    it "slices" $ do
          "\"Hello World\"(0, 5)" ~=?? Ok "\"Hello\""
          "\"Hello World\"(2, 4)" ~=?? Ok "\"ll\""
          "\"Hello World\"(0, -4)" ~=?? Ok "\"Hello W\""
          "\"Hello World\"(-4, -1)" ~=?? Ok "\"orl\""
          "\"Hello, World\"(2, null)" ~=?? Ok "\"llo, World\""
          "\"Hello! World\"(null, 5)" ~=?? Ok "\"Hello\""
          "\"Hello World\"(null, 0)" ~=?? Ok "\"\""
          "\"Hello\"(null, null)" ~=?? Ok "\"Hello\""
          "\"Hello World\"(-1, null)" ~=?? Ok "\"d\""

