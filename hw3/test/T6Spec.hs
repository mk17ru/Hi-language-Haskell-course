{-# LANGUAGE QuasiQuotes #-}
module T6Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "bytes tests" $ do
    it "simple" $ do
      "pack-bytes([ 3, 255, 158, 32 ]) " ~=?? Ok "[# 03 ff 9e 20 #]"
      "pack-bytes(range(30, 40))" ~=?? Ok "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
      "unpack-bytes([# 10 20 30 #])  " ~=?? Ok "[ 16, 32, 48 ]"
      "encode-utf8(\"Hello!\")" ~=?? Ok "[# 48 65 6c 6c 6f 21 #]"
      "decode-utf8([# 48 65 6c 6c 6f #])" ~=?? Ok "\"Hello\""
      "decode-utf8([# c3 28 #])" ~=?? Ok "null"
      "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" ~=?? Ok "[# 01 02 03 #]"
      "[# 00 ff #] + [# 01 e3 #]" ~=?? Ok "[# 00 ff 01 e3 #]"
      "decode-utf8([# 48 65 6c 6c 6f #])" ~=?? Ok "\"Hello\""
    it "error bytes" $ do
          "[# 0 #]" ~=?? ParseError ""
          "[# 0 1 #]" ~=?? ParseError ""
          "[# 4324 #]" ~=?? ParseError ""
          "[# 4 #]" ~=?? ParseError ""
    it "parse bytes" $ do
      "[#         #]" ~=?? Ok "[# #]"
      "[#11#]" ~=?? Ok "[# 11 #]"
      "[##]" ~=?? Ok "[# #]"
      "[# 11#]" ~=?? Ok "[# 11 #]"
      "[#12 #]" ~=?? Ok "[# 12 #]"
    it "zip" $ do
     "zip(encode-utf8(\"Hello, World!\" * 1000))" ~=?? Ok "[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]"
    it "opers" $ do
     "[# 00 ff #] + [# 01 e3 #]" ~=?? Ok "[# 00 ff 01 e3 #]"
     "[# 00 ff #] * 3" ~=?? Ok "[# 00 ff 00 ff 00 ff #]"
    it "errors" $ do
      "[# 11 #] * 3.2" ~=?? EvalError HiErrorInvalidArgument
      "[# 11 #] * -2" ~=?? EvalError HiErrorInvalidArgument
      "[# 44 24 #] * 0" ~=?? EvalError HiErrorInvalidArgument


