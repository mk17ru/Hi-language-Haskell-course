{-# LANGUAGE QuasiQuotes #-}
module T10Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "time" $ do
    it "echo" $ do
      "echo" ~=?? Ok "echo"
      "echo(\"Hello\")!" ~=?? Ok "null"
      "echo(\"Hello\")" ~=?? Ok "echo(\"Hello\")"
    it "or and" $ do
      "\"Hello\"(0) || \"Z\"" ~=?? Ok "\"H\""
      "\"Hello\"(99) || \"Z\"" ~=?? Ok "\"Z\""
      "if(2 == 2, echo(\"OK\")!, echo(\"WTF\")!)" ~=?? Ok "null"
      "true || echo(\"Don't do this\")!" ~=?? Ok "true"
      "false && echo(\"Don't do this\")!" ~=?? Ok "false"
      "[# 00 ff #] && echo(\"Just do it\")!" ~=?? Ok "null"
      "cwd!!" ~=?? EvalError HiErrorInvalidArgument
      "echo(\"hello\")!!" ~=?? EvalError HiErrorInvalidArgument


