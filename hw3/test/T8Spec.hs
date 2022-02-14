{-# LANGUAGE QuasiQuotes #-}
module T8Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "time" $ do
    it "time" $ do
      "parse-time(\"2021-01-01 00:00:00 UTC\")" ~=?? Ok "parse-time(\"2021-01-01 00:00:00 UTC\")"
      "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" ~=?? Ok "parse-time(\"2022-01-01 00:00:00 UTC\")"
      "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")" ~=?? Ok "3.351843755"
      "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000" ~=?? Ok "parse-time(\"2021-12-15 00:16:40 UTC\")"