{-# LANGUAGE QuasiQuotes #-}
module T11Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "dict" $ do
    it ". and !" $ do
      "{ \"key\": 1}.key" ~=?? Ok "1"
      "{ \"key\": {\"b\" : 1}}.key        .b" ~=?? Ok "1"
      "{ \"key\": {\"b\" : 1}}     .key.b" ~=?? Ok "1"
      "{ \"key\": {\"b\" : 1}}     .key    .b" ~=?? Ok "1"
      "{ \"key\": {\"b\" : 1}}     .key. b!" ~=?? ParseError ""
      "{ \"key\": 2}. key" ~=?? ParseError ""
    it "opers" $ do
      "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 }) " ~=?? Ok "{ 1: [ \"x\", \"z\" ], 2: [ \"y\" ] }"
      "count(\"Hello World\").o" ~=?? Ok "2"
      "count([# 58 58 58 4f 58 #])" ~=?? Ok "{ 79: 1, 88: 4 }"
      "count(\"XXXOX\")" ~=?? Ok "{ \"O\": 1, \"X\": 4 }"
      "values({ \"width\": 120, \"height\": 80 }) " ~=?? Ok "[ 80, 120 ]"