{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Streaming as Q
import Data.Function ((&))
import Data.Monoid ((<>))
import GHC.RTS.Events
import GHC.RTS.Events.Streaming
import qualified Streaming.Prelude as S
import System.Environment (getArgs)
import System.IO (IOMode(..), stdout, withFile)

printEventsStreaming :: FilePath -> IO ()
printEventsStreaming path = withFile path ReadMode $ \h -> do
      Q.hGetContentsN 1024 h
      & void . decodeEventLog
      & S.map (\event -> buildEvent' event <> "\n")
      & Q.concatBuilders
      & BB.hPutBuilder stdout

main :: IO ()
main = do
    [path] <- getArgs
    printEventsStreaming path
