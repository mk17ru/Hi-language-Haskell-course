module Main where

import HW3.Base
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import HW3.Action
import System.Console.Haskeline
import Data.Either (fromRight)
import Data.Set.Internal (Set, fromList)
import Control.Monad.Cont (liftIO)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi>"
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          let res = parse input
          outputStrLn $ show res
          case res of
            Left err -> outputStrLn $ show err
            Right ans -> do
              value <- liftIO $ runHIO (eval ans) (fromList [AllowRead, AllowWrite, AllowTime])
              case value of
                Left er -> outputStrLn $ show er
                Right val -> outputStrLn $ show (prettyValue val)
          loop
