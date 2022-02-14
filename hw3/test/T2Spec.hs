{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T2Spec
  ( spec
  ) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog
import Text.RawString.QQ

import Data.Char (toLower)

import HW3.Base
import SpecS

spec :: Spec
spec = do
  describe "bools" $ do
    it "simple" $ do
      "not(true)" ~=?? Ok "false"
      "and(true, false)" ~=?? Ok "false"
      "or(true, false)" ~=?? Ok "true"
    it "compare" $ do
      "equals(200, 200)" ~=?? Ok "true"
      "equals(4, 3)" ~=?? Ok "false"
      "equals(false, false)" ~=?? Ok "true"
      "less-than(200, 200)" ~=?? Ok "false"
      "less-than(4, 3)" ~=?? Ok "false"
      "less-than(3, 4)" ~=?? Ok "true"
      "less-than(false, false)" ~=?? Ok "false"
      "equals(less-than(40, 30), not-less-than(40, 30))" ~=?? Ok "false"

