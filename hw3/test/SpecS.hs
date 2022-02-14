{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module SpecS
  ( module HW3.Base
  , module SpecS
  , module Test.Hspec
  ) where

import Control.Exception
import Control.Monad.Identity
import Data.Either.Combinators (rightToMaybe)
import Data.Function
import Data.List (intercalate)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric (showEFloat)
import System.IO.Unsafe
import Test.Hspec
import Test.Hspec.Hedgehog (Gen, MonadTest, Range, failure, hedgehog, (===), annotate)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import HW3.Base
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty

#ifndef TEST_NO_HI_MONAD
import HW3.Action
import Data.Sequence (fromList, Seq (Empty))
import Data.Foldable (toList)
#endif

data TestRes
  = ParseError String
  | EvalError HiError
  | Ok String
#if HI_TEST_UPTO >= 7
  | Perm HiPermission
#endif
  deriving (Show)

instance Eq TestRes where
  ParseError _ == ParseError _ = True
  Ok a == Ok b = ((==) `on` filter (/= '\n')) a b
  EvalError a == EvalError b = a == b
#if HI_TEST_UPTO >= 7
  Perm a == Perm b = a == b
#endif
  _ == _ = False

getParsed :: String -> Maybe HiExpr
getParsed = rightToMaybe . parse

#if HI_TEST_UPTO >= 7
instance HiMonad Identity where
  runAction = const $ return HiValueNull

unwrapHIO :: Set.Set HiPermission -> HIO EvalRes -> TestRes
unwrapHIO perm HIO {..} =
  unsafePerformIO $ do
    res <- (Right <$> runHIO perm)
      `catch` (\(PermissionRequired ex) -> return $ Left ex)
    case res of
      Left err -> return $ Perm err
      Right res -> return $ matchEval res
#endif

type EvalRes = Either HiError HiValue

matchEval :: EvalRes -> TestRes
matchEval (Left  err) = EvalError err
matchEval (Right res) = Ok $ show $ prettyValue res

testEvalM :: HiMonad him => (him EvalRes -> TestRes) -> String -> TestRes
testEvalM unwrap s =
  case parse s of
    Left err -> ParseError $ errorBundlePretty err
    Right expr -> unwrap $ eval expr

testEval :: String -> TestRes
testEval = testEvalM (matchEval . runIdentity)

testEvalExpr :: (MonadTest m, HasCallStack) => HiExpr -> m TestRes
testEvalExpr e = do
  let eString = showExpr e
  annotate eString
  return $ testEval eString

infix 1 ~=??
(~=??) :: HasCallStack => String -> TestRes -> Expectation
b ~=?? a = testEval b `shouldBe` a

infix 1 ~=?!
(~=?!) :: String -> TestRes -> Expectation
b ~=?! a = testEval b `shouldNotBe` a

infix 1 ~=!!
(~=!!) :: MonadTest m => HiExpr -> TestRes -> m ()
expr ~=!! val = do
  res <- testEvalExpr expr
  res === val

ourRange :: Integral a => Range a
ourRange = Range.linear 0 1000

emptyTest :: Spec
emptyTest = describe "NOT IMPLEMENTED" $ it "fail" $ hedgehog failure

genExpr :: Gen HiExpr
genExpr = Gen.frequency
  [ (3, HiExprValue <$> genValue)
  , (1, HiExprApply <$> genExpr <*> Gen.list (Range.linear 1 3) genExpr)]

genValue :: Gen HiValue
genValue = Gen.choice
  [ genNum
  , genFun
  , genBool
  , genString
  , genSeq]

genNum :: Gen HiValue
genNum = HiValueNumber <$> ((%) <$> (Gen.integral ourRange) <*> (Gen.constant 1))

genBool :: Gen HiValue
genBool = HiValueBool <$> Gen.bool

genFun :: Gen HiValue
genFun = HiValueFunction <$> Gen.element
  [ HiFunDiv
  , HiFunMul
  , HiFunAdd
  , HiFunSub
  , HiFunNot
  , HiFunAnd
  , HiFunOr
  , HiFunLessThan
  , HiFunGreaterThan
  , HiFunEquals
  , HiFunNotLessThan
  , HiFunNotGreaterThan
  , HiFunNotEquals
  , HiFunIf
  , HiFunLength
  , HiFunToUpper
  , HiFunToLower
  , HiFunReverse
  , HiFunTrim
  , HiFunList
  , HiFunRange
  , HiFunFold
  , HiFunPackBytes
  , HiFunUnpackBytes
  , HiFunEncodeUtf8
  , HiFunDecodeUtf8
  , HiFunZip
  , HiFunUnzip
  , HiFunSerialise
  , HiFunDeserialise ]

genString :: Gen HiValue
genString = HiValueString <$> Gen.text (Range.linear 0 100) Gen.alphaNum

genSeq :: Gen HiValue
genSeq = (HiValueList . fromList) <$> Gen.list (Range.linear 0 5) genValue

makeOp :: HiFun -> [HiExpr] -> HiExpr
makeOp op args = HiExprApply (HiExprValue $ HiValueFunction op) args

makeBinaryOp :: HiFun -> HiValue -> HiValue -> HiExpr
makeBinaryOp op lhs rhs = makeOp op $ map HiExprValue [lhs, rhs]

-- only for gen!
showExpr :: HiExpr -> String
showExpr (HiExprValue v)      = case v of
  HiValueNumber num -> show (numerator num)
  HiValueFunction f -> case f of
    HiFunDiv            -> "div"
    HiFunMul            -> "mul"
    HiFunAdd            -> "add"
    HiFunSub            -> "sub"
    HiFunNot            -> "not"
    HiFunAnd            -> "and"
    HiFunOr             -> "or"
    HiFunLessThan       -> "less-than"
    HiFunGreaterThan    -> "greater-than"
    HiFunEquals         -> "equals"
    HiFunNotLessThan    -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals      -> "not-equals"
    HiFunIf             -> "if"
    HiFunLength         -> "length"
    HiFunToUpper        -> "to-upper"
    HiFunToLower        -> "to-lower"
    HiFunReverse        -> "reverse"
    HiFunTrim           -> "trim"
    HiFunList           -> "list"
    HiFunRange          -> "range"
    HiFunFold           -> "fold"
    HiFunPackBytes      -> "pack-bytes"
    HiFunUnpackBytes    -> "unpack-bytes"
    HiFunEncodeUtf8     -> "encode-utf8"
    HiFunDecodeUtf8     -> "decode-utf8"
    HiFunZip            -> "zip"
    HiFunUnzip          -> "unzip"
    HiFunSerialise      -> "serialise"
    HiFunDeserialise    -> "deserialise"
  HiValueBool b     -> if b then "true" else "false"
  HiValueString t -> show t
  HiValueNull -> "null"
  HiValueList Empty -> "[ ]"
  HiValueList lst -> "[ " ++ intercalate ", " (map (showExpr . HiExprValue) $ toList lst) ++ " ]"
  _ -> undefined
showExpr (HiExprApply f args) = showExpr f ++ "(" ++ intercalate ", " (map showExpr args) ++ ")"
showExpr _ = undefined


testSameEval :: MonadTest m => HiExpr -> HiExpr -> m ()
testSameEval e1 e2 = do
  e1res <- testEvalExpr e1
  e2res <- testEvalExpr e2
  case (e1res, e2res) of
    (EvalError _, EvalError _) -> pure ()
    _                          -> e1res === e2res
