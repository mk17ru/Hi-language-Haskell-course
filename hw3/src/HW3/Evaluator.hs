{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module HW3.Evaluator
  ( eval,
  )
where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (ExceptT, foldM, lift, runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Ch (concat, drop, empty, foldr, foldr', group, index, intercalate, pack, singleton, split, take, unpack)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable as F (toList)
import Data.Map (Map)
import Data.Map as M (empty, foldrWithKey, fromList, insertWith, lookup, map, toAscList)
import Data.Maybe (fromJust, isNothing)
import Data.Ratio (Rational)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (Empty, (:<|)), (><))
import qualified Data.Sequence as S (drop, empty, fromList, index, length, reverse, singleton, take)
import Data.Set (singleton, union)
import Data.Text (Text)
import Data.Text as T (chunksOf, drop, empty, index, intercalate, length, pack, reverse, singleton, strip, take, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.IO ()
import GHC.Real hiding (Rational)
import HW3.Action ()
import HW3.Base
import HW3.Common
import Numeric ()
import Text.Read (readMaybe)

-- | class for union operations with Lists, ByteStrings and Text
class (Ord a, Semigroup a) => CommonList a where
  llength :: a -> Int
  rreverse :: a -> a
  ddrop :: Int -> a -> a
  ttake :: Int -> a -> a
  toHiValue :: a -> HiValue
  iindex :: a -> Int -> HiValue
  eempty :: a -> HiValue

instance CommonList Text where
  llength = T.length
  rreverse = T.reverse
  ddrop = T.drop
  ttake = T.take
  toHiValue = HiValueString
  iindex str shift = HiValueString $ T.singleton (T.index str shift)
  eempty _ = HiValueString T.empty

instance CommonList ByteString where
  llength = Prelude.length . Ch.unpack
  rreverse = Ch.pack . Prelude.reverse . Ch.unpack
  ddrop = Ch.drop
  ttake = Ch.take
  toHiValue = HiValueBytes
  iindex l shift = HiValueNumber $ toRational $ toInteger (Ch.index l shift)
  eempty _ = HiValueBytes Ch.empty

instance CommonList (Seq HiValue) where
  llength = S.length
  rreverse = S.reverse
  ddrop = S.drop
  ttake = S.take
  toHiValue = HiValueList
  iindex = S.index
  eempty _ = HiValueList S.empty

-- | data for use the commonlist in pattern matching
data Listable = forall a. CommonList a => Listable a

-- | convert commonlist to listable
packListable :: CommonList a => a -> Listable
packListable = Listable

-- | outer function for evaluation
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalCommon expr

-- | Common function for evaluation
evalCommon :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalCommon (HiExprRun expr) =
  do
    res <- evalCommon expr
    case res of
      (HiValueAction action) -> lift $ runAction action
      otherwise -> throwError HiErrorInvalidArgument
evalCommon (HiExprDict mp) = HiValueDict . M.fromList <$> mapM evalEntry mp
evalCommon (HiExprApply (HiExprValue val) args) = operation val args
evalCommon (HiExprApply (HiExprDict val) args) = do
  inner <- evalCommon (HiExprDict val)
  operation inner args
evalCommon (HiExprApply (HiExprApply innerFunc innerArgs) args) =
  do
    inner <- evalCommon (HiExprApply innerFunc innerArgs)
    operation inner args
evalCommon (HiExprValue num) = pure num
evalCommon _ = throwError HiErrorInvalidFunction

evalEntry :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalEntry (x, y) = (,) <$> evalCommon x <*> evalCommon y

-- | compute arguments for functions or apply lazy
operation :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
operation func list =
  if isLazy func
    then computeLazy func list
    else do
      evals <- mapM evalCommon list
      case getArity func of
        0 -> computeList func evals
        1 -> applyUnary func evals
        2 -> applyBinary func evals
        otherwise -> throwError HiErrorInvalidFunction

computeLazy :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
computeLazy (HiValueFunction HiFunIf) [cond, x, y] =
  do
    condition <- evalCommon cond
    case condition of
      (HiValueBool True) -> evalCommon x
      (HiValueBool False) -> evalCommon y
      otherwise -> throwError HiErrorInvalidArgument
computeLazy (HiValueFunction HiFunAnd) [x, y] = computeAnd x y
computeLazy (HiValueFunction HiFunOr) [x, y] = computeOr x y
computeLazy func args =
  if getArity func /= Prelude.length args
    then throwError HiErrorArityMismatch
    else throwError HiErrorInvalidArgument

-- | apply
applyUnary :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyUnary val [x] = computeUnary val x
applyUnary _ _ = throwError HiErrorArityMismatch

-- | apply binary
applyBinary :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyBinary val [x, y] = computeBinary val x y
applyBinary _ _ = throwError HiErrorArityMismatch

-- | common function for unary list, string, bytestring operations
commonListUnary :: HiMonad m => HiValue -> (Listable -> ExceptT HiError m HiValue) -> ExceptT HiError m HiValue
commonListUnary (HiValueString str) func = func (packListable str)
commonListUnary (HiValueList l) func = func (packListable l)
commonListUnary (HiValueBytes l) func = func (packListable l)
commonListUnary _ _ = throwError HiErrorInvalidArgument

-- | common function for binary list, string, bytestring operations
commonListBinary ::
  HiMonad m =>
  HiValue -> -- | first arg
  HiValue -> -- | second arg
  (Listable -> HiValue -> ExceptT HiError m HiValue) -> -- | binary function
  ExceptT HiError m HiValue 
commonListBinary (HiValueString x) y func = func (packListable x) y
commonListBinary (HiValueBytes x) y func = func (packListable x) y
commonListBinary (HiValueList x) y func = func (packListable x) y
commonListBinary _ _ _ = throwError HiErrorInvalidArgument

-- | get list length
listLength :: (HiMonad m) => Listable -> ExceptT HiError m HiValue
listLength (Listable list) = return (HiValueNumber (toRational (llength list)))

-- | reverse list
listReverse :: HiMonad m => Listable -> ExceptT HiError m HiValue
listReverse (Listable l) = return $ toHiValue $ rreverse l

-- | compute Listable operation (which have different number of args)
computeList :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
computeList (HiValueFunction HiFunList) args = return (HiValueList $ S.fromList args)
computeList (HiValueFunction HiFunFold) args = evalFold args
computeList fun [x] = computeUnary fun x
computeList fun [x, y] = computeBinary fun x y
computeList _ _ = throwError HiErrorInvalidFunction

-- | fold evaluation for binary functions
evalFold :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
evalFold [fun, list] =
  if getArity fun /= 2
    then throwError HiErrorInvalidArgument
    else case list of
      HiValueList Empty -> return $ HiValueNull
      HiValueList (h :<| t) -> foldM (computeBinary fun) h t
      otherwise -> throwError HiErrorInvalidArgument
evalFold _ = throwError HiErrorInvalidArgument

-- | eval unary operation
computeUnary ::
  HiMonad m =>
  HiValue -> -- | first arg
  HiValue -> -- | second arg
  ExceptT HiError m HiValue
computeUnary (HiValueFunction HiFunNot) (HiValueBool x) = return (HiValueBool (not x))
computeUnary (HiValueFunction HiFunPackBytes) (HiValueList t) = packList (F.toList t)
computeUnary (HiValueFunction HiFunUnpackBytes) (HiValueBytes t) = unpackList t
computeUnary (HiValueFunction HiFunLength) l = commonListUnary l listLength
computeUnary (HiValueFunction HiFunReverse) l = commonListUnary l listReverse
computeUnary (HiValueFunction HiFunRead) (HiValueString s) = return $ HiValueAction $ HiActionRead $ T.unpack s
computeUnary (HiValueFunction HiFunMkDir) (HiValueString s) = return $ HiValueAction $ HiActionMkDir $ T.unpack s
computeUnary (HiValueFunction HiFunChDir) (HiValueString s) = return $ HiValueAction $ HiActionChDir $ T.unpack s
computeUnary (HiValueFunction HiFunEncodeUtf8) (HiValueString str) = return $ HiValueBytes $ encodeUtf8 str
computeUnary (HiValueFunction HiFunDecodeUtf8) (HiValueBytes bytes) =
  do
    let result = decodeUtf8' bytes
    case result of
      (Left _) -> return HiValueNull
      (Right val) -> return $ HiValueString val
computeUnary (HiValueFunction HiFunZip) (HiValueBytes bytes) =
  return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} (fromStrict bytes)
computeUnary (HiValueFunction HiFunUnzip) (HiValueBytes bytes) =
  return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams (fromStrict bytes)
computeUnary (HiValueFunction HiFunToUpper) (HiValueString text) = return (HiValueString (T.toUpper text))
computeUnary (HiValueFunction HiFunToLower) (HiValueString text) = return (HiValueString (T.toLower text))
computeUnary (HiValueFunction HiFunTrim) (HiValueString text) = return (HiValueString (T.strip text))
computeUnary (HiValueFunction HiFunSerialise) x = return $ HiValueBytes $ toStrict $ serialise x
computeUnary (HiValueFunction HiFunDeserialise) (HiValueBytes x) =
  do
    let result = deserialiseOrFail (fromStrict x)
    case result of
      (Left _) -> return HiValueNull
      (Right val) -> return val
computeUnary (HiValueFunction HiFunParseTime) (HiValueString text) =
  do
    case readTime text of
      Nothing -> return HiValueNull
      (Just time) -> return $ HiValueTime time
computeUnary (HiValueFunction HiFunEcho) (HiValueString str) = return $ HiValueAction $ HiActionEcho str
computeUnary (HiValueDict dict) el =
  do
    let findEl = M.lookup el dict
    case findEl of
      Nothing -> return HiValueNull
      Just x -> return x
computeUnary (HiValueFunction HiFunCount) (HiValueList l) = countEval (F.toList l)
computeUnary (HiValueFunction HiFunCount) (HiValueString l) = countEval (fmap HiValueString (chunksOf 1 l))
computeUnary (HiValueFunction HiFunCount) (HiValueBytes bs) = countEval (unpackBytes bs)
computeUnary (HiValueFunction HiFunInvert) (HiValueDict d) = return $ HiValueDict $ M.map HiValueList (invertMap d)
computeUnary (HiValueFunction HiFunKeys) dict = getFromMap dict fst
computeUnary (HiValueFunction HiFunValues) dict = getFromMap dict snd
computeUnary l arg = commonListBinary l arg evalGetElem

getFromMap ::
  HiMonad m =>
  HiValue -> -- | dict
  ((HiValue, HiValue) -> HiValue) -> -- | getter func
  ExceptT HiError m HiValue
getFromMap (HiValueDict dict) func = return $ HiValueList $ S.fromList $ Prelude.map func (M.toAscList dict)
getFromMap _ _ = throwError HiErrorInvalidArgument

-- | return HiValueDict (HiValue, HiValueNumber number of appearance)
countEval :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
countEval l = return $ HiValueDict $ fmap (HiValueNumber . toRational) (countMapEval l)

-- | return map (HiValue, number of appearance)
countMapEval :: [HiValue] -> Map HiValue Int
countMapEval = Prelude.foldr unionMapValues M.empty
  where
    unionMapValues :: Ord a => a -> Map a Int -> Map a Int
    unionMapValues val = insertWith (+) val 1

-- | unpack bytes to list
unpackBytes :: ByteString -> [HiValue]
unpackBytes bs = fmap (HiValueNumber . toRational . toInteger) (Ch.unpack bs)

-- | swap keys and values
invertMap :: Map HiValue HiValue -> Map HiValue (Seq HiValue)
invertMap = M.foldrWithKey unionMapValues M.empty
  where
    unionMapValues :: Ord a => a -> a -> Map a (Seq a) -> Map a (Seq a)
    unionMapValues key val = insertWith (><) val (S.singleton key)

-- | function for time read
readTime :: Text -> Maybe UTCTime
readTime text = readMaybe (T.unpack text)

-- | get element
evalGetElem :: (HiMonad m) => Listable -> HiValue -> ExceptT HiError m HiValue
evalGetElem (Listable l) (HiValueNumber num)
  | not (checkInteger num) = throwError HiErrorInvalidArgument
  | shift < 0 || llength l <= shift = return HiValueNull
  | otherwise = return $ iindex l shift
  where
    shift = fromInteger $ numerator num
evalGetElem _ _ = throwError HiErrorInvalidArgument

-- | eval binary operation
computeBinary :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
computeBinary (HiValueFunction HiFunRange) = getBinaryOper HiFunRange
computeBinary (HiValueFunction HiFunRand) = getBinaryOper HiFunRand
computeBinary (HiValueFunction HiFunEcho) = getBinaryOper HiFunEcho
computeBinary (HiValueFunction HiFunDiv) = getBinaryOper HiFunDiv
computeBinary (HiValueFunction HiFunAdd) = getBinaryOper HiFunAdd
computeBinary (HiValueFunction HiFunMul) = getBinaryOper HiFunMul
computeBinary (HiValueFunction HiFunSub) = getBinaryOper HiFunSub
computeBinary (HiValueFunction HiFunAnd) = getBinaryOper HiFunAnd
computeBinary (HiValueFunction HiFunOr) = getBinaryOper HiFunOr
computeBinary (HiValueFunction HiFunEquals) = getBinaryOper HiFunEquals
computeBinary (HiValueFunction HiFunNotEquals) = getBinaryOper HiFunNotEquals
computeBinary (HiValueFunction HiFunLessThan) = getBinaryOper HiFunLessThan
computeBinary (HiValueFunction HiFunGreaterThan) = getBinaryOper HiFunGreaterThan
computeBinary (HiValueFunction HiFunNotLessThan) = getBinaryOper HiFunNotLessThan
computeBinary (HiValueFunction HiFunNotGreaterThan) = getBinaryOper HiFunNotGreaterThan
computeBinary (HiValueFunction HiFunWrite) = getBinaryOper HiFunWrite
computeBinary (HiValueString str) = evalSlice str
computeBinary (HiValueList list) = evalSlice list
computeBinary (HiValueBytes list) = evalSlice list
computeBinary _ = \_ _ -> throwError HiErrorInvalidFunction

-- | eval binary functions
getBinaryOper :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
getBinaryOper HiFunEquals x y = return (HiValueBool (x == y))
getBinaryOper HiFunNotEquals x y = return (HiValueBool (x /= y))
getBinaryOper HiFunLessThan x y = return (HiValueBool (x < y))
getBinaryOper HiFunGreaterThan x y = return (HiValueBool (x > y))
getBinaryOper HiFunNotLessThan x y = return (HiValueBool (x >= y))
getBinaryOper HiFunNotGreaterThan x y = return (HiValueBool (x <= y))
getBinaryOper HiFunAnd x y = computeAnd (HiExprValue x) (HiExprValue y)
getBinaryOper HiFunOr x y = computeOr (HiExprValue x) (HiExprValue y)
getBinaryOper HiFunDiv (HiValueNumber x) (HiValueNumber y) =
  if y == 0
    then throwError HiErrorDivideByZero
    else return (HiValueNumber (x / y))
getBinaryOper HiFunDiv (HiValueString x) (HiValueString y) = return (HiValueString $ T.intercalate (T.pack "/") [x, y])
getBinaryOper HiFunMul (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x * y)
getBinaryOper HiFunAdd (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x + y)
getBinaryOper HiFunAdd (HiValueTime t) (HiValueNumber (n :% d)) =
  if checkInteger (n :% d)
    then return $ HiValueTime $ addUTCTime (fromInteger n) t
    else throwError HiErrorInvalidArgument
getBinaryOper HiFunSub (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x - y)
getBinaryOper HiFunSub (HiValueTime t1) (HiValueTime t2) = return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
getBinaryOper HiFunRange (HiValueNumber x) (HiValueNumber y) = return $ HiValueList $ S.fromList $ fmap HiValueNumber [x .. y]
getBinaryOper HiFunWrite (HiValueString path) (HiValueString s) = computeWrite path (encodeUtf8 s)
getBinaryOper HiFunWrite (HiValueString path) (HiValueBytes bs) = computeWrite path bs
getBinaryOper HiFunRand (HiValueNumber (nx :% dx)) (HiValueNumber (ny :% dy)) =
  if checkInteger (nx :% dx) && checkInteger (ny :% dy)
    then return $ HiValueAction $ HiActionRand (fromInteger nx) (fromInteger ny)
    else throwError HiErrorInvalidArgument
getBinaryOper HiFunMul x y = commonListBinary x y evalStimes
getBinaryOper HiFunAdd (HiValueString x) (HiValueString y) = return $ toHiValue (x <> y)
getBinaryOper HiFunAdd (HiValueList x) (HiValueList y) = return $ toHiValue (x <> y)
getBinaryOper HiFunAdd (HiValueBytes x) (HiValueBytes y) = return $ toHiValue (x <> y)
getBinaryOper _ _ _ = throwError HiErrorInvalidArgument

-- |convert HiValue to Byte
toByte :: HiValue -> Maybe Integer
toByte (HiValueNumber x) =
  if checkInteger x && x <= 255 && x >= 0
    then Just (numerator x)
    else Nothing
toByte _ = Nothing

-- | common function for create write action
computeWrite :: HiMonad m => Text -> ByteString -> ExceptT HiError m HiValue
computeWrite path bs = return $ HiValueAction $ HiActionWrite (T.unpack path) bs

-- | function for create and
computeAnd :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
computeAnd x y =
  do
    res <- evalCommon x
    case res of
      (HiValueBool False) -> return $ HiValueBool False
      HiValueNull -> return HiValueNull
      otherwise -> evalCommon y

-- | function for create or
computeOr :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
computeOr x y =
  do
    res <- evalCommon x
    case res of
      (HiValueBool False) -> evalCommon y
      HiValueNull -> evalCommon y
      otherwise -> return res

-- | function for unpacking list
unpackList :: HiMonad m => ByteString -> ExceptT HiError m HiValue
unpackList bs = return $ HiValueList (S.fromList $ unpackBytes bs)

-- | functiom for packing list
packList :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
packList args =
  do
    let bytes = fmap toByte args
    if any isNothing bytes
      then throwError HiErrorInvalidArgument
      else return $ HiValueBytes $ Ch.pack $ fmap (fromIntegral . fromJust) bytes

-- | evaluation stimes for CommonList class
evalStimes :: (HiMonad m) => Listable -> HiValue -> ExceptT HiError m HiValue
evalStimes (Listable x) (HiValueNumber num) =
  do
    if not $ checkInteger num && num > 0
      then throwError HiErrorInvalidArgument
      else return $ toHiValue $ stimes (numerator num) x
evalStimes _ _ = throwError HiErrorInvalidArgument

-- | evaluation slices for CommonList class, pattern matching null values
evalSlice :: (HiMonad m, CommonList a) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalSlice slice (HiValueNumber start) (HiValueNumber end) = evalSlice' slice start end
evalSlice slice HiValueNull HiValueNull = evalSlice' slice 0 (toRational (llength slice))
evalSlice slice HiValueNull (HiValueNumber end) = evalSlice' slice 0 end
evalSlice slice (HiValueNumber start) HiValueNull = evalSlice' slice start (toRational (llength slice))
evalSlice _ _ _ = throwError HiErrorInvalidArgument

-- | function for evalSlice
evalSlice' :: (HiMonad m, CommonList a) => a -> Rational -> Rational -> ExceptT HiError m HiValue
evalSlice' str start end
  | not (checkInteger start) || not (checkInteger end) = throwError HiErrorInvalidArgument
  | otherwise =
    do
      let l = evalBorder shiftS len 0
      let r = evalBorder shiftE len 1
      if l > r
        then return $ eempty str
        else return $ toHiValue $ ttake (r - l + 1) $ ddrop l str
  where
    shiftS = fromInteger $ numerator start
    shiftE = fromInteger $ numerator end
    len = llength str

-- | function for eval border
evalBorder :: Int -> Int -> Int -> Int
evalBorder start len sh
  | start < 0 =
    if start >= - len
      then start + len - sh
      else 0
  | otherwise =
    if start > len
      then len
      else start - sh
