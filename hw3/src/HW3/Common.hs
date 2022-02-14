module HW3.Common
  ( getArity,
    funName,
    checkInteger,
    actionName,
    isLazy,
  )
where

import Data.Maybe (isJust)
import Data.Ratio (Rational)
import GHC.Real hiding (Rational)
import HW3.Base
import Prelude.SafeEnum (fromEnum)

{-| 
  get Function arity
  0 - different number of args
  1 - unary
  2 - binary
  ...
  -1 - can't have any arguments
|-}
getArity :: HiValue -> Int
getArity (HiValueFunction HiFunFold) = 0
getArity (HiValueFunction HiFunList) = 0
getArity (HiValueDict _) = 0
getArity (HiValueString _) = 0
getArity (HiValueBytes _) = 0
getArity (HiValueList _) = 0
getArity (HiValueFunction HiFunNot) = 1
getArity (HiValueFunction HiFunLength) = 1
getArity (HiValueFunction HiFunToUpper) = 1
getArity (HiValueFunction HiFunToLower) = 1
getArity (HiValueFunction HiFunReverse) = 1
getArity (HiValueFunction HiFunTrim) = 1
getArity (HiValueFunction HiFunPackBytes) = 1
getArity (HiValueFunction HiFunUnpackBytes) = 1
getArity (HiValueFunction HiFunEncodeUtf8) = 1
getArity (HiValueFunction HiFunDecodeUtf8) = 1
getArity (HiValueFunction HiFunZip) = 1
getArity (HiValueFunction HiFunUnzip) = 1
getArity (HiValueFunction HiFunSerialise) = 1
getArity (HiValueFunction HiFunDeserialise) = 1
getArity (HiValueFunction HiFunRead) = 1
getArity (HiValueFunction HiFunMkDir) = 1
getArity (HiValueFunction HiFunChDir) = 1
getArity (HiValueFunction HiFunParseTime) = 1
getArity (HiValueFunction HiFunEcho) = 1
getArity (HiValueFunction HiFunCount) = 1
getArity (HiValueFunction HiFunKeys) = 1
getArity (HiValueFunction HiFunValues) = 1
getArity (HiValueFunction HiFunInvert) = 1
getArity (HiValueAction _) = 1
getArity (HiValueFunction HiFunNotLessThan) = 2
getArity (HiValueFunction HiFunNotGreaterThan) = 2
getArity (HiValueFunction HiFunNotEquals) = 2
getArity (HiValueFunction HiFunLessThan) = 2
getArity (HiValueFunction HiFunGreaterThan) = 2
getArity (HiValueFunction HiFunEquals) = 2
getArity (HiValueFunction HiFunDiv) = 2
getArity (HiValueFunction HiFunMul) = 2
getArity (HiValueFunction HiFunAdd) = 2
getArity (HiValueFunction HiFunSub) = 2
getArity (HiValueFunction HiFunAnd) = 2
getArity (HiValueFunction HiFunOr) = 2
getArity (HiValueFunction HiFunRange) = 2
getArity (HiValueFunction HiFunWrite) = 2
getArity (HiValueFunction HiFunRand) = 2
getArity (HiValueFunction HiFunIf) = 3
getArity _ = -1

{-| 
  show for HiFun
  Maybe instance show HiFun is better, but we don't know how user would like to use show on HiFun
|-}
funName :: HiFun -> String
funName HiFunDiv = "div"
funName HiFunMul = "mul"
funName HiFunAdd = "add"
funName HiFunSub = "sub"
funName HiFunNot = "not"
funName HiFunAnd = "and"
funName HiFunOr = "or"
funName HiFunLessThan = "less-than"
funName HiFunGreaterThan = "greater-than"
funName HiFunEquals = "equals"
funName HiFunNotLessThan = "not-less-than"
funName HiFunNotGreaterThan = "not-greater-than"
funName HiFunNotEquals = "not-equals"
funName HiFunIf = "if"
funName HiFunLength = "length"
funName HiFunToUpper = "to-upper"
funName HiFunToLower = "to-lower"
funName HiFunReverse = "reverse"
funName HiFunTrim = "trim"
funName HiFunList = "list"
funName HiFunRange = "range"
funName HiFunFold = "fold"
funName HiFunPackBytes = "pack-bytes"
funName HiFunUnpackBytes = "unpack-bytes"
funName HiFunEncodeUtf8 = "encode-utf8"
funName HiFunDecodeUtf8 = "decode-utf8"
funName HiFunZip = "zip"
funName HiFunUnzip = "unzip"
funName HiFunSerialise = "serialise"
funName HiFunDeserialise = "deserialise"
funName HiFunRead = "read"
funName HiFunWrite = "write"
funName HiFunMkDir = "mkdir"
funName HiFunChDir = "cd"
funName HiFunParseTime = "parse-time"
funName HiFunRand = "rand"
funName HiFunEcho = "echo"
funName HiFunCount = "count"
funName HiFunKeys = "keys"
funName HiFunValues = "values"
funName HiFunInvert = "invert"

-- | show for HiAction
actionName :: HiAction -> String
actionName (HiActionRead _) = "read"
actionName (HiActionWrite _ _) = "write"
actionName (HiActionMkDir _) = "mkdir"
actionName (HiActionChDir _) = "cd"
actionName HiActionCwd = "cwd"
actionName HiActionNow = "now"
actionName (HiActionRand _ _) = "rand"
actionName (HiActionEcho _) = "echo"

-- | check if number is Int
checkInteger :: Rational -> Bool
checkInteger (n :% d) = d == 1 && isJust (Prelude.SafeEnum.fromEnum n)

-- |return if it is Lazy function
isLazy :: HiValue -> Bool
isLazy (HiValueFunction HiFunIf) = True
isLazy (HiValueFunction HiFunOr) = True
isLazy (HiValueFunction HiFunAnd) = True
isLazy _ = False
