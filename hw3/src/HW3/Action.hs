{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module HW3.Action
  ( HiPermission (..),
    PermissionException (..),
    HIO (..),
  )
where

import Control.Exception.Base (Exception, throwIO)
import Control.Monad (ap)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.Sequence as Seq (fromList)
import Data.Set (Set)
import qualified Data.Set as S (difference, elemAt, empty, singleton)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW3.Base
--import Control.Concurrent(threadDelay)

import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    getCurrentDirectory,
    listDirectory,
    setCurrentDirectory,
  )
import System.FilePath ((</>))
import System.Random.Stateful (getStdRandom, uniformR)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a} deriving (Functor)

instance Applicative HIO where
  pure arg = HIO (\_ -> pure arg)
  p <*> q = ap p q

-- | join realisation for HIO
join :: HIO (HIO s) -> HIO s
join state = HIO $ \s ->
  do
    x <- runHIO state s
    runHIO x s

instance Monad HIO where
  m >>= f = join $ fmap f m

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction action = HIO $ \s ->
    do
      let perms = S.difference perm s
      if null perms
        then commonAction action
        else throwIO $ PermissionRequired (S.elemAt 0 perms)
    where
      perm = getPermission action

-- |  Do eval parsed Action
commonAction :: HiAction -> IO HiValue
commonAction HiActionNow = HiValueTime <$> getCurrentTime
commonAction (HiActionRand x y) =
  do
    num <- getStdRandom (uniformR (x, y))
    return $ HiValueNumber $ toRational num
commonAction (HiActionRead filepath) =
  do
    d <- getCurrentDirectory
    let name = d </> filepath
    isDir <- doesDirectoryExist name
    if isDir
      then do
        ds <- listDirectory name
        return $ HiValueList $ Seq.fromList $ fmap (HiValueString . T.pack) ds
      else do
        bs <- BS.readFile name
        let result = decodeUtf8' bs
        case result of
          (Left _) -> return $ HiValueBytes bs
          (Right val) -> return $ HiValueString val
commonAction (HiActionWrite filepath bs) =
  do
    d <- getCurrentDirectory
    BS.writeFile (d </> filepath) bs
    return HiValueNull
commonAction (HiActionChDir filepath) =
  do
    d <- getCurrentDirectory
    setCurrentDirectory (d </> filepath)
    return HiValueNull
commonAction (HiActionMkDir filepath) =
  do
    d <- getCurrentDirectory
    let name = d </> filepath
    createDirectoryIfMissing True name
    return HiValueNull
commonAction HiActionCwd = HiValueString . T.pack <$> getCurrentDirectory
commonAction (HiActionEcho text) =
  do
    putStrLn (T.unpack text)
    return HiValueNull

-- | get Permission for current action
getPermission :: HiAction -> Set HiPermission
getPermission (HiActionRead _) = S.singleton AllowRead
getPermission (HiActionWrite _ _) = S.singleton AllowWrite
getPermission (HiActionMkDir _) = S.singleton AllowWrite
getPermission (HiActionChDir _) = S.singleton AllowRead
getPermission HiActionCwd = S.singleton AllowRead
getPermission HiActionNow = S.singleton AllowTime
getPermission (HiActionEcho _) = S.singleton AllowWrite
getPermission (HiActionRand _ _) = S.empty
