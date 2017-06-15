{-# LANGUAGE OverloadedStrings #-}

-- | These benchmarks convert events from binary to text format.
module Main where

import Control.Monad (when, void)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Streaming as Q
import Data.Function ((&))
import Data.Monoid ((<>))
import Debug.Trace (traceEventIO)
import GHC.RTS.Events
import qualified GHC.RTS.Events.Incremental as Incremental
import GHC.RTS.Events.Streaming
import qualified Streaming.Prelude as S
import System.Directory (doesFileExist)
import System.FilePath (dropExtension, takeFileName)
import System.Environment (getArgs, getExecutablePath)
import System.IO (IOMode(..), withFile)
import System.Process (callProcess)

-- | Converts events using package 'streaming'.
printEventsStreaming :: FilePath -> FilePath -> IO ()
printEventsStreaming path pathOut = withFile path ReadMode $ \h ->
    withFile pathOut WriteMode $ \hOut ->
      Q.hGetContentsN 1024 h
      & void . decodeEventLog
      & S.map (\event -> buildEvent' event <> "\n")
      & Q.concatBuilders
      & BB.hPutBuilder hOut

-- | Converts events by reading them into a lazy list.
printEventsIncremental1 :: FilePath -> FilePath -> IO ()
printEventsIncremental1 path pathOut = withFile path ReadMode $ \h -> do
    withFile pathOut WriteMode $ \hOut -> do
      input <- BL.hGetContents h
      case Incremental.readEventLog input of
        Left err -> fail err
        Right (EventLog _hdr (Data evs), _) ->
            map (\event -> buildEvent' event <> "\n") evs
          & mconcat
          & BB.hPutBuilder hOut

-- | Amount of events to produce if the logFile is missing
nEvents :: Int
nEvents = 5000000

main :: IO ()
main = do
    cmdargs <- getArgs
    exePath <- getExecutablePath

    let logFile = dropExtension (takeFileName exePath) ++ ".eventlog"
    case cmdargs of
      "events" : _ ->
        sequence_ $ replicate nEvents (traceEventIO "test event")
      "incremental1" : _ ->
        printEventsIncremental1 logFile "incremental1.txt"
      "incremental" : _ ->
        -- Converts events without reading them into an intermediate stream or
        -- list.
        printEventsIncremental False logFile "incremental.txt"
      "streaming" : _ ->
        printEventsStreaming logFile "streaming.txt"
      _ -> do
        generateEventlog logFile
        run "incremental"
        run "incremental1"
        run "streaming"
  where
    run :: String -> IO ()
    run bench = do
      exePath <- getExecutablePath
      putStrLn ""
      putStrLn $ bench ++ " ..."
      putStrLn ""
      callProcess exePath [bench, "+RTS", "-s"]

    -- Only generates events if the eventlog file is missing.
    generateEventlog :: FilePath -> IO ()
    generateEventlog logFile = do
      exists <- doesFileExist logFile
      when (not exists) $ do
        exePath <- getExecutablePath
        putStrLn "generating eventlog ..."
        callProcess exePath ["events", "+RTS", "-la"]
        putStrLn "done"
