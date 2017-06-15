{-# LANGUAGE CPP,BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=150 #-}
{-
 -   Parser functions for GHC RTS EventLog framework.
 -}

module GHC.RTS.Events (
       -- * The event log types
       EventLog(..),
       Header(..),
       Data(..),
       EventType(..),
       Event(..),
       EventInfo(..),
       ThreadStopStatus(..),
       CapsetType(..),
       Timestamp,
       ThreadId,
       TaskId,
       KernelThreadId(..),
       EventTypeNum,
       EventTypeDesc,
       EventTypeSize,
       BlockSize,
       Capset,
       StringId,

       -- * Reading and writing event logs
       readEventLogFromFile,
       writeEventLogToFile,

       -- * Utilities
       sortEvents,
       buildEventTypeMap,

       -- * Printing
       printEventsIncremental,
       showEventInfo, buildEventInfo,
       showThreadStopStatus,
       ppEventLog, ppEventType,
       ppEvent, buildEvent, buildEvent',
  ) where

{- Libraries. -}
import Control.Applicative
import Control.Concurrent hiding (ThreadId)
import qualified Data.Binary.Put as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Foldable (foldMap)
import Data.Function hiding (id)
import Data.List
import Data.Monoid ((<>))
import System.IO
import Prelude hiding (gcd, rem, id)

import GHC.RTS.EventTypes
import GHC.RTS.Events.Binary
import GHC.RTS.Events.Incremental

-- | Read an entire eventlog file. It returns an error message if it
-- encouters an error while decoding.
--
-- Note that it doesn't fail if it consumes all input in the middle of decoding
-- of an event.
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile path = fmap fst . readEventLog <$> BL.readFile path

-- | Read an eventlog file and pretty print it to the given output file.
printEventsIncremental
  :: Bool -- ^ Follow the file or not
  -> FilePath
  -> FilePath
  -> IO ()
printEventsIncremental follow path pathOut =
  withFile path ReadMode $ \h ->
  withFile pathOut WriteMode $ \hOut ->
    (hPrintEventsIncremental follow h hOut)

-- | Read an eventlog from the Handle and pretty print it to the given handle
hPrintEventsIncremental
  :: Bool -- ^ Follow the handle or not
  -> Handle
  -> Handle
  -> IO ()
hPrintEventsIncremental follow hdl hdlOut = go decodeEventLog
  where
    go decoder = case decoder of
      Produce event decoder' -> do
        BB.hPutBuilder hdlOut $ buildEvent' event <> "\n"
        go decoder'
      Consume k -> do
        chunk <- B.hGetSome hdl 1024
        if
          | not (B.null chunk) -> go $ k chunk
          | follow -> threadDelay 1000000 >> go decoder
          | otherwise -> return ()
      Done {} -> return ()
      Error _ err -> fail err


-- | Writes the 'EventLog' to file. The log is expected to __NOT__ have 'EventBlock'
-- markers/events - the parsers no longer emit them and they are handled behind
-- the scenes.
writeEventLogToFile :: FilePath -> EventLog -> IO ()
writeEventLogToFile fp = BL.writeFile fp . serialiseEventLog


-- | Serialises an 'EventLog' back to a 'ByteString', usually for writing it
-- back to a file.
serialiseEventLog :: EventLog -> BL.ByteString
serialiseEventLog el@(EventLog _ (Data events)) =
  P.runPut $ putEventLog blockedEl
  where
    eventsMap = capSplitEvents events
    blockedEventsMap = IM.mapWithKey addBlockMarker eventsMap
    blockedEl = el{dat = Data blockedEvents}
    blockedEvents = IM.foldr (++) [] blockedEventsMap

-- Gets the Capability of an event in numeric form
getIntCap :: Event -> Int
getIntCap Event{evCap = cap} =
  case cap of
  Just capNo -> capNo
  Nothing    -> -1

-- Creates an IntMap of the events with capability number as the key.
-- Key -1 indicates global (capless) event
capSplitEvents :: [Event] -> IM.IntMap [Event]
capSplitEvents evts = capSplitEvents' evts IM.empty

capSplitEvents' :: [Event] -> IM.IntMap [Event] -> IM.IntMap [Event]
capSplitEvents' evts imap =
  case evts of
  (x:xs) -> capSplitEvents' xs (IM.insertWith (++) (getIntCap x) [x] imap)
  []     -> imap

-- Adds a block marker to the beginnng of a list of events, annotated with
-- its capability. All events are expected to belong to the same cap.
addBlockMarker :: Int -> [Event] -> [Event]
addBlockMarker cap evts =
  (Event startTime (EventBlock endTime cap sz) (mkCap cap)) : sortedEvts
  where
    sz = fromIntegral . BL.length $ P.runPut $ mapM_ putEvent evts
    startTime = case sortedEvts of
      (x:_) -> evTime x
      [] -> error "Cannot add block marker to an empty list of events"
    sortedEvts = sortEvents evts
    endTime = evTime $ last sortedEvts

-- -----------------------------------------------------------------------------
-- Utilities
sortEvents :: [Event] -> [Event]
sortEvents = sortBy (compare `on` evTime)

buildEventTypeMap :: [EventType] -> IntMap EventType
buildEventTypeMap etypes =
  IM.fromList [ (fromIntegral (num t),t) | t <- etypes ]

-----------------------------------------------------------------------------
-- Some pretty-printing support

showEventInfo :: EventInfo -> String
showEventInfo = BL8.unpack . BB.toLazyByteString . buildEventInfo

buildEventInfo :: EventInfo -> BB.Builder
buildEventInfo spec' =
    case spec' of
        EventBlock end_time cap _block_events ->
          "event block: cap " <> BB.intDec cap
          <> ", end time: " <> builder end_time <> "\n"
        CreateThread thread ->
          "creating thread " <> builder thread
        RunThread thread ->
          "running thread " <> builder thread
        StopThread thread status ->
          "stopping thread " <> builder thread
          <> " (" <> BB.stringUtf8 (showThreadStopStatus status) <> ")"
        ThreadRunnable thread ->
          "thread " <> builder thread <> " is runnable"
        MigrateThread thread newCap  ->
          "migrating thread " <> builder thread
          <> " to cap " <> BB.intDec newCap
        CreateSparkThread sparkThread ->
          "creating spark thread " <> builder sparkThread
        SparkCounters crt dud ovf cnv fiz gcd rem ->
          "spark stats: "
          <> builder crt <> " created, "
          <> builder cnv <> " converted, "
          <> builder rem <> " remaining ("
          <> builder ovf <> " overflowed, "
          <> builder dud <> " dud, "
          <> builder gcd <> " GC'd, "
          <> builder fiz <> " fizzled)"
        SparkCreate ->
          "spark created"
        SparkDud ->
          "dud spark discarded"
        SparkOverflow ->
          "overflowed spark discarded"
        SparkRun ->
          "running a local spark"
        SparkSteal victimCap ->
          "stealing a spark from cap " <> BB.intDec victimCap
        SparkFizzle ->
          "spark fizzled"
        SparkGC ->
          "spark GCed"
        TaskCreate (TaskId taskId) cap tid ->
          "task 0x" <> BB.word64Hex taskId
          <> " created on cap " <> BB.intDec cap
          <>" with OS kernel thread " <> builder (kernelThreadId tid)
        TaskMigrate (TaskId taskId) cap new_cap ->
          "task 0x" <> BB.word64Hex taskId
          <> " migrated from cap " <> BB.intDec cap
          <> " to cap " <> BB.intDec new_cap
        TaskDelete (TaskId taskId) ->
          "task 0x" <> BB.word64Hex taskId <> " deleted"
        WakeupThread thread otherCap ->
          "waking up thread " <> builder thread
          <> " on cap " <> BB.intDec otherCap
        ThreadLabel thread label ->
          "thread " <> builder thread
          <> " has label \"" <> BB.byteString label <> "\""
        RequestSeqGC ->
          "requesting sequential GC"
        RequestParGC ->
          "requesting parallel GC"
        StartGC ->
          "starting GC"
        EndGC ->
          "finished GC"
        GCWork ->
          "GC working"
        GCIdle ->
          "GC idle"
        GCDone ->
          "GC done"
        GlobalSyncGC ->
          "all caps stopped for GC"
        GCStatsGHC{..} ->
          "GC stats for heap capset " <> builder heapCapset
          <> ": generation " <> BB.intDec gen <> ", "
          <> builder copied <> " bytes copied, "
          <> builder slop <> " bytes slop, "
          <> builder frag <> " bytes fragmentation, "
          <> BB.intDec parNThreads <> " par threads, "
          <> builder parMaxCopied <> " bytes max par copied, "
          <> builder parTotCopied <> " bytes total par copied"
        HeapAllocated{..} ->
          "allocated on heap capset " <> builder heapCapset
          <> ": " <> builder allocBytes <> " total bytes till now"
        HeapSize{..} ->
          "size of heap capset " <> builder heapCapset
          <> ": " <> builder sizeBytes <> " bytes"
        HeapLive{..} ->
          "live data in heap capset " <> builder heapCapset
          <> ": " <> builder liveBytes <> " bytes"
        HeapInfoGHC{..} ->
          "heap stats for heap capset " <> builder heapCapset
          <> ": generations " <> BB.intDec gens <> ", "
          <> builder maxHeapSize <> " bytes max heap size, "
          <> builder allocAreaSize <> " bytes alloc area size, "
          <> builder mblockSize <> " bytes mblock size, "
          <> builder blockSize <> " bytes block size"
        CapCreate{cap} ->
          "created cap " <> BB.intDec cap
        CapDelete{cap} ->
          "deleted cap " <> BB.intDec cap
        CapDisable{cap} ->
          "disabled cap " <> BB.intDec cap
        CapEnable{cap} ->
          "enabled cap " <> BB.intDec cap
        Message msg ->
          BB.byteString msg
        UserMessage msg ->
          BB.byteString msg
        UserMarker markername ->
          "marker: " <> BB.byteString markername
        CapsetCreate cs ct ->
          "created capset " <> builder cs
          <> " of type " <> BB.stringUtf8 (show ct)
        CapsetDelete cs ->
          "deleted capset " <> builder cs
        CapsetAssignCap cs cp ->
          "assigned cap " <> BB.intDec cp <> " to capset " <> builder cs
        CapsetRemoveCap cs cp ->
          "removed cap " <> BB.intDec cp <> " from capset " <> builder cs
        OsProcessPid cs pid ->
          "capset " <> builder cs <> ": pid " <> builder pid
        OsProcessParentPid cs ppid ->
          "capset " <> builder cs <> ": parent pid " <> builder ppid
        WallClockTime cs sec nsec ->
          "capset " <> builder cs <> ": wall clock time "
          <> builder sec <> "s "
          <> builder nsec <> "ns (unix epoch)"
        RtsIdentifier cs i ->
          "capset " <> builder cs
          <> ": RTS version \"" <> BB.byteString i <> "\""
        ProgramArgs cs args ->
          "capset " <> builder cs
          <> ": args: " <> BB.stringUtf8 (show args)
        ProgramEnv cs env ->
          "capset " <> builder cs
          <> ": env: " <> BB.stringUtf8 (show env)
        UnknownEvent n ->
          "Unknown event type " <> builder n

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus NoStatus = "No stop thread status"
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
showThreadStopStatus BlockedOnMVar  = "blocked on an MVar"
showThreadStopStatus BlockedOnMVarRead = "blocked reading an MVar"
showThreadStopStatus BlockedOnBlackHole = "blocked on a black hole"
showThreadStopStatus (BlockedOnBlackHoleOwnedBy target) =
          "blocked on black hole owned by thread " ++ show target
showThreadStopStatus BlockedOnRead = "blocked on I/O read"
showThreadStopStatus BlockedOnWrite = "blocked on I/O write"
showThreadStopStatus BlockedOnDelay = "blocked on threadDelay"
showThreadStopStatus BlockedOnSTM = "blocked in STM retry"
showThreadStopStatus BlockedOnDoProc = "blocked on asyncDoProc"
showThreadStopStatus BlockedOnCCall = "blocked in a foreign call"
showThreadStopStatus BlockedOnCCall_Interruptible = "blocked in a foreign call"
showThreadStopStatus BlockedOnMsgThrowTo = "blocked in throwTo"
showThreadStopStatus ThreadMigrating = "thread migrating"

ppEventLog :: EventLog -> String
ppEventLog = BL8.unpack . BB.toLazyByteString . buildEventLog

buildEventLog :: EventLog -> BB.Builder
buildEventLog (EventLog (Header ets) (Data es)) =
  "Event Types:\n"
  <> foldMap (\evType -> buildEventType evType <> "\n") ets
  <> "\n"
  <> "Events:\n"
  <> foldMap (\ev -> buildEvent imap ev <> "\n") sorted
  where
    imap = buildEventTypeMap ets
    sorted = sortEvents es

ppEventType :: EventType -> String
ppEventType = BL8.unpack . BB.toLazyByteString . buildEventType

buildEventType :: EventType -> BB.Builder
buildEventType (EventType num dsc msz) =
  builder num <> ": "
  <> builder dsc <> " (size "
  <> maybe "variable" builder msz <> ")"

-- | Pretty prints an 'Event', with clean handling for 'UnknownEvent'
ppEvent :: IntMap EventType -> Event -> String
ppEvent imap = BL8.unpack . BB.toLazyByteString . buildEvent imap

buildEvent :: IntMap EventType -> Event -> BB.Builder
buildEvent imap Event {..} =
  builder evTime
  <> ": "
  <> maybe "" (\c -> "cap " <> BB.intDec c <> ": ") evCap
  <> case evSpec of
    UnknownEvent{ ref=ref } ->
      maybe "" (builder . desc) $ IM.lookup (fromIntegral ref) imap
    _ -> buildEventInfo evSpec

buildEvent' :: Event -> BB.Builder
buildEvent' Event {..} =
   builder evTime
   <> ": "
   <> maybe "" (\c -> "cap " <> BB.intDec c <> ": ") evCap
   <> case evSpec of
     UnknownEvent{ ref=ref } ->
      "Unknown Event (ref: " <> builder ref <> ")"
     _ -> buildEventInfo evSpec
