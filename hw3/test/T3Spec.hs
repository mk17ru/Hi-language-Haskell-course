{-# LANGUAGE QuasiQuotes #-}
module T3Spec (spec) where

import HW3.Base
import SpecS
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "operations" $ do
    it "easy" $ do
      "1 + 1" ~=?? Ok "2"
      "2 * 4" ~=?? Ok "8"
      "6 / 2" ~=?? Ok "3"
      "6 - 2" ~=?? Ok "4"
      "6.2 - 2.4" ~=?? Ok "3.8"
      "-434" ~=?? Ok "-434"
    it "hard" $ do
      "144 * 43242 - 324234 + 43423 * (1 + 4 / 3 + 3 - 2)" ~=?? Ok "6047357 + 1/3"
      "2 + 2 * 3 == (2 + 2) * 3" ~=?? Ok "false"
      "10 == 2*5 && 143 == 11*13" ~=?? Ok "true"
      "43.45 * 432.4" ~=?? Ok "18787.78"
      "15.2 == 3*5 || 143 == 11*13" ~=?? Ok "true"
      "15 == 3*5 || 143 == 12*13" ~=?? Ok "true"
      "15 == 4*5 || 143 == 12*13" ~=?? Ok "false"
      "15 >= 3*5" ~=?? Ok "true"
      "15 <= 3*5" ~=?? Ok "true"
      "15 > 3*5" ~=?? Ok "false"
      "15 < 3*5" ~=?? Ok "false"
      "15 /= 3*5" ~=?? Ok "false"




