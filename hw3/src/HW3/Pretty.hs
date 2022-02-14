{-# LANGUAGE BlockArguments #-}

module HW3.Pretty
  ( prettyValue,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as Ch (unpack)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.Foldable as F (toList)
import qualified Data.List (intercalate)
import qualified Data.Map as M (toList)
import Data.Maybe (isNothing)
import Data.Ratio (Rational)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import GHC.Real hiding (Rational)
import HW3.Base
import HW3.Common
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | Pretty output
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction func) = pretty $ funName func
prettyValue (HiValueString text) = pretty $ show text
prettyValue (HiValueList l) = brackett "[" "]" (concatDocs $ fmap prettyValue (F.toList l))
prettyValue (HiValueBytes bs) = brackett "[#" "#]" $ pretty (prettyBytes bs)
prettyValue (HiValueAction (HiActionRead path)) = printAction (HiActionRead path) (pretty $ show path)
prettyValue (HiValueAction (HiActionMkDir path)) = printAction (HiActionMkDir path) (pretty $ show path)
prettyValue (HiValueAction (HiActionChDir path)) = printAction (HiActionChDir path) (pretty $ show path)
prettyValue (HiValueAction (HiActionEcho path)) = printAction (HiActionEcho path) (pretty $ show path)
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueTime t) = pretty $ "parse-time(" ++ show (show t) ++ ")"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionWrite path byte)) =
  printAction (HiActionWrite path byte) (sepPrint "," (viaShow path) (prettyValue (HiValueBytes byte)))
prettyValue (HiValueAction (HiActionRand x y)) = printAction (HiActionRand x y) (sepPrint "," (pretty x) (pretty y))
prettyValue (HiValueDict dict) =
  brackett "{" "}" $ concatDocs (fmap printEntry (M.toList dict))
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueNumber (n :% d))
  | d == 1 = pretty n
  | isNothing isInf = pretty (formatScientific Fixed Nothing num)
  | q == 0 = printFrac n d
  | otherwise = pretty (show q) <+> getSign r <+> printFrac (abs r) d
  where
    (num, isInf) = fromRationalRepetendUnlimited (n :% d)
    (q, r) = quotRem n d
    printFrac :: Integer -> Integer -> Doc AnsiStyle
    printFrac numer denom = pretty numer <> pretty "/" <> pretty denom

-- |  pretty Dict entries
printEntry :: (HiValue, HiValue) -> Doc AnsiStyle
printEntry (x, y) = prettyValue x <> pretty ":" <+> prettyValue y

-- | pretty actions
printAction :: HiAction -> Doc AnsiStyle -> Doc AnsiStyle
printAction action inner = pretty (actionName action ++ "(") <> inner <> pretty ")"

-- | separate docs by separ 
sepPrint :: String -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
sepPrint separ x y = x <> pretty separ <+> y

-- |  concate Docs by comma
concatDocs :: [Doc ann] -> Doc ann
concatDocs = concatWith (\x y -> x <> pretty ", " <> y)

-- |  get sign of integer
getSign :: Integer -> Doc ann
getSign num =
  if num >= 0
    then pretty "+"
    else pretty "-"

-- | pretty Bytestring
prettyBytes :: ByteString -> String
prettyBytes bs = prettyBytesInteger (fmap toInteger (Ch.unpack bs))

-- | translate integer bytes to string bytes
prettyBytesInteger :: [Integer] -> String
prettyBytesInteger args = unwords (fmap prettyByte args)

-- | pretty byte : 01 0f ff ss
prettyByte :: Integer -> String
prettyByte x =
  if x < 16
    then "0" ++ showHex x ""
    else showHex x ""

-- | pretty between with brackets
brackett :: String -> String -> Doc AnsiStyle -> Doc AnsiStyle
brackett start end doc =
  do
    if Prelude.null (show doc)
      then pretty start <+> pretty end
      else pretty start <+> doc <+> pretty end
