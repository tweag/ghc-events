{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.RTS.Events.Binary
  ( -- * Readers
    getHeader
  , getEvent
  , standardParsers
  , ghc7Parsers
  , post782StopParser

  -- * Writers
  , putEventLog
  , putHeader
  , putEvent
  ) where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Binary
import qualified Data.Binary.Get as G
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Maybe
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes
import Prelude hiding (gcd, rem, id)

#define EVENTLOG_CONSTANTS_ONLY
#include <rts/EventLogFormat.h>

getEventType :: Get EventType
getEventType = do
           etNum <- get
           size <- get :: Get EventTypeSize
           let etSize = if size == 0xffff then Nothing else Just size
           -- 0xffff indicates variable-sized event
           etDescLen <- get :: Get EventTypeDescLen
           etDesc <- EventTypeDesc <$> G.getByteString (fromIntegral etDescLen)
           etExtraLen <- get :: Get Word32
           G.skip (fromIntegral etExtraLen)
           ete <- get :: Get Marker
           when (ete /= EVENT_ET_END) $
              fail "Event Type end marker not found."
           return (EventType etNum etDesc etSize)

getHeader :: Get Header
getHeader = do
            hdrb <- get :: Get Marker
            when (hdrb /= EVENT_HEADER_BEGIN) $
                 fail "Header begin marker not found"
            hetm <- get :: Get Marker
            when (hetm /= EVENT_HET_BEGIN) $
                 fail "Header Event Type begin marker not found"
            ets <- getEventTypes
            emark <- get :: Get Marker
            when (emark /= EVENT_HEADER_END) $
                 fail "Header end marker not found"
            db <- get :: Get Marker
            when (db /= EVENT_DATA_BEGIN) $
                  fail "My Data begin marker not found"
            return $ Header ets
     where
      getEventTypes :: Get [EventType]
      getEventTypes = do
          m <- get :: Get Marker
          case m of
             EVENT_ET_BEGIN -> do
                  et <- getEventType
                  nextET <- getEventTypes
                  return (et : nextET)
             EVENT_HET_END ->
                  return []
             _ ->
                  fail "Malformed list of Event Types in header"

getEvent :: EventParsers -> Get (Maybe Event)
getEvent (EventParsers parsers) = do
  etRef <- get :: Get EventTypeNum
  if etRef == EVENT_DATA_END
     then return Nothing
     else do !evTime   <- get
             evSpec <- parsers ! fromIntegral etRef
             return $ Just Event { evCap = undefined, .. }

--
-- standardEventParsers.
--
standardParsers :: [EventParser EventInfo]
standardParsers = [
 (FixedSizeParser EVENT_BLOCK_MARKER (sz_block_size + sz_time + sz_cap) (do -- (size, end_time, cap)
      block_size <- get :: Get BlockSize
      end_time <- get :: Get Timestamp
      c <- get :: Get CapNo
      return EventBlock { end_time   = end_time,
                          cap        = fromIntegral c,
                          block_size = ((fromIntegral block_size) -
                                        (fromIntegral sz_block_event))
                        }
   )),

 (simpleEvent EVENT_REQUEST_SEQ_GC RequestSeqGC),

 (simpleEvent EVENT_REQUEST_PAR_GC RequestParGC),

 (simpleEvent EVENT_GC_START StartGC),

 (simpleEvent EVENT_GC_WORK GCWork),

 (simpleEvent EVENT_GC_IDLE GCIdle),

 (simpleEvent EVENT_GC_DONE GCDone),

 (simpleEvent EVENT_GC_END EndGC),

 (simpleEvent EVENT_GC_GLOBAL_SYNC GlobalSyncGC),

 (FixedSizeParser EVENT_GC_STATS_GHC (sz_capset + 2 + 5*8 + 4) (do  -- (heap_capset, generation, copied_bytes, slop_bytes, frag_bytes, par_n_threads, par_max_copied, par_tot_copied)
      heapCapset   <- get
      gen          <- get :: Get Word16
      copied       <- get :: Get Word64
      slop         <- get :: Get Word64
      frag         <- get :: Get Word64
      parNThreads  <- get :: Get Word32
      parMaxCopied <- get :: Get Word64
      parTotCopied <- get :: Get Word64
      return GCStatsGHC{ gen = fromIntegral gen
                       , parNThreads = fromIntegral parNThreads
                       , ..}
 )),

 (FixedSizeParser EVENT_HEAP_ALLOCATED (sz_capset + 8) (do  -- (heap_capset, alloc_bytes)
      heapCapset <- get
      allocBytes <- get
      return HeapAllocated{..}
 )),

 (FixedSizeParser EVENT_HEAP_SIZE (sz_capset + 8) (do  -- (heap_capset, size_bytes)
      heapCapset <- get
      sizeBytes  <- get
      return HeapSize{..}
 )),

 (FixedSizeParser EVENT_HEAP_LIVE (sz_capset + 8) (do  -- (heap_capset, live_bytes)
      heapCapset <- get
      liveBytes  <- get
      return HeapLive{..}
 )),

 (FixedSizeParser EVENT_HEAP_INFO_GHC (sz_capset + 2 + 4*8) (do  -- (heap_capset, n_generations, max_heap_size, alloc_area_size, mblock_size, block_size)
      heapCapset    <- get
      gens          <- get :: Get Word16
      maxHeapSize   <- get :: Get Word64
      allocAreaSize <- get :: Get Word64
      mblockSize    <- get :: Get Word64
      blockSize     <- get :: Get Word64
      return HeapInfoGHC{gens = fromIntegral gens, ..}
 )),

 (FixedSizeParser EVENT_CAP_CREATE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapCreate{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_DELETE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapDelete{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_DISABLE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapDisable{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_ENABLE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapEnable{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAPSET_CREATE (sz_capset + sz_capset_type) (do -- (capset, capset_type)
      cs <- get
      ct <- fmap toCapsetType get
      return CapsetCreate{capset=cs,capsetType=ct}
   )),

 (FixedSizeParser EVENT_CAPSET_DELETE sz_capset (do -- (capset)
      cs <- get
      return CapsetDelete{capset=cs}
   )),

 (FixedSizeParser EVENT_CAPSET_ASSIGN_CAP (sz_capset + sz_cap) (do -- (capset, cap)
      cs <- get
      cp <- get :: Get CapNo
      return CapsetAssignCap{capset=cs,cap=fromIntegral cp}
   )),

 (FixedSizeParser EVENT_CAPSET_REMOVE_CAP (sz_capset + sz_cap) (do -- (capset, cap)
      cs <- get
      cp <- get :: Get CapNo
      return CapsetRemoveCap{capset=cs,cap=fromIntegral cp}
   )),

 (FixedSizeParser EVENT_OSPROCESS_PID (sz_capset + sz_pid) (do -- (capset, pid)
      cs <- get
      pd <- get
      return OsProcessPid{capset=cs,pid=pd}
   )),

 (FixedSizeParser EVENT_OSPROCESS_PPID (sz_capset + sz_pid) (do -- (capset, ppid)
      cs <- get
      pd <- get
      return OsProcessParentPid{capset=cs,ppid=pd}
  )),

 (FixedSizeParser EVENT_WALL_CLOCK_TIME (sz_capset + 8 + 4) (do -- (capset, unix_epoch_seconds, nanoseconds)
      cs <- get
      s  <- get
      ns <- get
      return WallClockTime{capset=cs,sec=s,nsec=ns}
  )),

 (VariableSizeParser EVENT_LOG_MSG (do -- (msg)
      num <- get :: Get EventTypeSize
      string <- getString num
      return Message{ msg = string }
   )),
 (VariableSizeParser EVENT_USER_MSG (do -- (msg)
      num <- get :: Get EventTypeSize
      string <- getString num
      return UserMessage{ msg = string }
   )),
    (VariableSizeParser EVENT_USER_MARKER (do -- (markername)
      num <- get :: Get EventTypeSize
      string <- getString num
      return UserMarker{ markername = string }
   )),
 (VariableSizeParser EVENT_PROGRAM_ARGS (do -- (capset, [arg])
      num <- get :: Get EventTypeSize
      cs <- get
      string <- getString (num - sz_capset)
      return ProgramArgs{ capset = cs
                        , args = BS.split 0 string }
   )),
 (VariableSizeParser EVENT_PROGRAM_ENV (do -- (capset, [arg])
      num <- get :: Get EventTypeSize
      cs <- get
      string <- getString (num - sz_capset)
      return ProgramEnv{ capset = cs
                       , env = BS.split 0 string }
   )),
 (VariableSizeParser EVENT_RTS_IDENTIFIER (do -- (capset, str)
      num <- get :: Get EventTypeSize
      cs <- get
      string <- getString (num - sz_capset)
      return RtsIdentifier{ capset = cs
                          , rtsident = string }
   )),

 (VariableSizeParser EVENT_THREAD_LABEL (do -- (thread, str)
      num <- get :: Get EventTypeSize
      tid <- get
      str <- getString (num - sz_tid)
      return ThreadLabel{ thread      = tid
                        , threadlabel = str }
    ))
 ]

-- Parsers valid for GHC7 but not GHC6.
ghc7Parsers :: [EventParser EventInfo]
ghc7Parsers = [
 (FixedSizeParser EVENT_CREATE_THREAD sz_tid (do  -- (thread)
      t <- get
      return CreateThread{thread=t}
   )),

 (FixedSizeParser EVENT_RUN_THREAD sz_tid (do  --  (thread)
      t <- get
      return RunThread{thread=t}
   )),

 (FixedSizeParser EVENT_THREAD_RUNNABLE sz_tid (do  -- (thread)
      t <- get
      return ThreadRunnable{thread=t}
   )),

 (FixedSizeParser EVENT_MIGRATE_THREAD (sz_tid + sz_cap) (do  --  (thread, newCap)
      t  <- get
      nc <- get :: Get CapNo
      return MigrateThread{thread=t,newCap=fromIntegral nc}
   )),

 (FixedSizeParser EVENT_CREATE_SPARK_THREAD sz_tid (do  -- (sparkThread)
      st <- get :: Get ThreadId
      return CreateSparkThread{sparkThread=st}
   )),

 (FixedSizeParser EVENT_SPARK_COUNTERS (7*8) (do -- (crt,dud,ovf,cnv,gcd,fiz,rem)
      crt <- get :: Get Word64
      dud <- get :: Get Word64
      ovf <- get :: Get Word64
      cnv <- get :: Get Word64
      gcd <- get :: Get Word64
      fiz <- get :: Get Word64
      rem <- get :: Get Word64
      return SparkCounters{sparksCreated    = crt, sparksDud       = dud,
                           sparksOverflowed = ovf, sparksConverted = cnv,
                           -- Warning: order of fiz and gcd reversed!
                           sparksFizzled    = fiz, sparksGCd       = gcd,
                           sparksRemaining  = rem}
   )),

 (simpleEvent EVENT_SPARK_CREATE   SparkCreate),
 (simpleEvent EVENT_SPARK_DUD      SparkDud),
 (simpleEvent EVENT_SPARK_OVERFLOW SparkOverflow),
 (simpleEvent EVENT_SPARK_RUN      SparkRun),
 (FixedSizeParser EVENT_SPARK_STEAL sz_cap (do  -- (victimCap)
      vc <- get :: Get CapNo
      return SparkSteal{victimCap=fromIntegral vc}
   )),
 (simpleEvent EVENT_SPARK_FIZZLE   SparkFizzle),
 (simpleEvent EVENT_SPARK_GC       SparkGC),

 (FixedSizeParser EVENT_TASK_CREATE (sz_taskid + sz_cap + sz_kernel_tid) (do  -- (taskID, cap, tid)
      taskId <- get :: Get TaskId
      cap    <- get :: Get CapNo
      tid    <- get :: Get KernelThreadId
      return TaskCreate{ taskId, cap = fromIntegral cap, tid }
   )),
 (FixedSizeParser EVENT_TASK_MIGRATE (sz_taskid + sz_cap*2) (do  -- (taskID, cap, new_cap)
      taskId  <- get :: Get TaskId
      cap     <- get :: Get CapNo
      new_cap <- get :: Get CapNo
      return TaskMigrate{ taskId, cap = fromIntegral cap
                                , new_cap = fromIntegral new_cap
                        }
   )),
 (FixedSizeParser EVENT_TASK_DELETE (sz_taskid) (do  -- (taskID)
      taskId <- get :: Get TaskId
      return TaskDelete{ taskId }
   )),

 (FixedSizeParser EVENT_THREAD_WAKEUP (sz_tid + sz_cap) (do  -- (thread, other_cap)
      t <- get
      oc <- get :: Get CapNo
      return WakeupThread{thread=t,otherCap=fromIntegral oc}
   ))
 ]

-- parsers for GHC >= 7.8.3, always using block info field parser.
-- See [Stop status in GHC-7.8.2] in EventTypes.hs
post782StopParser :: EventParser EventInfo
post782StopParser =
 (FixedSizeParser EVENT_STOP_THREAD (sz_tid + sz_th_stop_status + sz_tid)
    (do
      -- (thread, status, info)
      t <- get
      s <- get :: Get RawThreadStopStatus
      i <- get :: Get ThreadId
      return StopThread{thread = t,
                        status = case () of
                                  _ | s > maxThreadStopStatus
                                    -> NoStatus
                                    | s == 8 {- XXX yeuch -}
                                      -- post-7.8.2: 8==BlockedOnBlackhole
                                    -> BlockedOnBlackHoleOwnedBy i
                                    | otherwise
                                    -> toThreadStopStatus s}
    ))

-----------------------------------------------------------

putE :: Binary a => a -> PutM ()
putE = put

putType :: EventTypeNum -> PutM ()
putType = putE

putCap :: Int -> PutM ()
putCap c = putE (fromIntegral c :: CapNo)

putMarker :: Word32 -> PutM ()
putMarker = putE

putEventLog :: EventLog -> PutM ()
putEventLog (EventLog hdr es) = do
    putHeader hdr
    putData es

putHeader :: Header -> PutM ()
putHeader (Header ets) = do
    putMarker EVENT_HEADER_BEGIN
    putMarker EVENT_HET_BEGIN
    mapM_ putEventType ets
    putMarker EVENT_HET_END
    putMarker EVENT_HEADER_END
 where
    putEventType (EventType n (EventTypeDesc d) msz) = do
        putMarker EVENT_ET_BEGIN
        putType n
        putE $ fromMaybe 0xffff msz
        putE (fromIntegral $ BS.length d :: EventTypeDescLen)
        putByteString d
        -- the event type header allows for extra data, which we don't use:
        putE (0 :: Word32)
        putMarker EVENT_ET_END

putData :: Data -> PutM ()
putData (Data es) = do
    putMarker EVENT_DATA_BEGIN -- Word32
    mapM_ putEvent es
    putType EVENT_DATA_END -- Word16

eventTypeNum :: EventInfo -> EventTypeNum
eventTypeNum e = case e of
    CreateThread {} -> EVENT_CREATE_THREAD
    RunThread {} -> EVENT_RUN_THREAD
    StopThread {} -> EVENT_STOP_THREAD
    ThreadRunnable {} -> EVENT_THREAD_RUNNABLE
    MigrateThread {} -> EVENT_MIGRATE_THREAD
    WakeupThread {} -> EVENT_THREAD_WAKEUP
    ThreadLabel {}  -> EVENT_THREAD_LABEL
    StartGC {} -> EVENT_GC_START
    EndGC {} -> EVENT_GC_END
    GlobalSyncGC {} -> EVENT_GC_GLOBAL_SYNC
    RequestSeqGC {} -> EVENT_REQUEST_SEQ_GC
    RequestParGC {} -> EVENT_REQUEST_PAR_GC
    CreateSparkThread {} -> EVENT_CREATE_SPARK_THREAD
    SparkCounters {} -> EVENT_SPARK_COUNTERS
    SparkCreate   {} -> EVENT_SPARK_CREATE
    SparkDud      {} -> EVENT_SPARK_DUD
    SparkOverflow {} -> EVENT_SPARK_OVERFLOW
    SparkRun      {} -> EVENT_SPARK_RUN
    SparkSteal    {} -> EVENT_SPARK_STEAL
    SparkFizzle   {} -> EVENT_SPARK_FIZZLE
    SparkGC       {} -> EVENT_SPARK_GC
    TaskCreate  {} -> EVENT_TASK_CREATE
    TaskMigrate {} -> EVENT_TASK_MIGRATE
    TaskDelete  {} -> EVENT_TASK_DELETE
    Message {} -> EVENT_LOG_MSG
    EventBlock {} -> EVENT_BLOCK_MARKER
    UserMessage {} -> EVENT_USER_MSG
    UserMarker  {} -> EVENT_USER_MARKER
    GCIdle {} -> EVENT_GC_IDLE
    GCWork {} -> EVENT_GC_WORK
    GCDone {} -> EVENT_GC_DONE
    GCStatsGHC{} -> EVENT_GC_STATS_GHC
    HeapAllocated{} -> EVENT_HEAP_ALLOCATED
    HeapSize{} -> EVENT_HEAP_SIZE
    HeapLive{} -> EVENT_HEAP_LIVE
    HeapInfoGHC{} -> EVENT_HEAP_INFO_GHC
    CapCreate{} -> EVENT_CAP_CREATE
    CapDelete{} -> EVENT_CAP_DELETE
    CapDisable{} -> EVENT_CAP_DISABLE
    CapEnable{} -> EVENT_CAP_ENABLE
    CapsetCreate {} -> EVENT_CAPSET_CREATE
    CapsetDelete {} -> EVENT_CAPSET_DELETE
    CapsetAssignCap {} -> EVENT_CAPSET_ASSIGN_CAP
    CapsetRemoveCap {} -> EVENT_CAPSET_REMOVE_CAP
    RtsIdentifier {} -> EVENT_RTS_IDENTIFIER
    ProgramArgs {} -> EVENT_PROGRAM_ARGS
    ProgramEnv {} -> EVENT_PROGRAM_ENV
    OsProcessPid {} -> EVENT_OSPROCESS_PID
    OsProcessParentPid{} -> EVENT_OSPROCESS_PPID
    WallClockTime{} -> EVENT_WALL_CLOCK_TIME
    UnknownEvent {} -> error "eventTypeNum UnknownEvent"

putEvent :: Event -> PutM ()
putEvent Event {..} = do
    putType (eventTypeNum evSpec)
    put evTime
    putEventSpec evSpec

putEventSpec :: EventInfo -> PutM ()
putEventSpec (EventBlock end cap sz) = do
    putE (fromIntegral (sz+24) :: BlockSize)
    putE end
    putE (fromIntegral cap :: CapNo)

putEventSpec (CreateThread t) =
    putE t

putEventSpec (RunThread t) =
    putE t

-- here we assume that ThreadStopStatus fromEnum matches the definitions in
-- EventLogFormat.h
-- The standard encoding is used here, which is wrong for eventlogs
-- produced by GHC-7.8.2 ([Stop status in GHC-7.8.2] in EventTypes.hs
putEventSpec (StopThread t s) = do
    putE t
    putE $ fromThreadStopStatus s
    putE $ case s of
            BlockedOnBlackHoleOwnedBy i -> i
            _                           -> 0

putEventSpec (ThreadRunnable t) =
    putE t

putEventSpec (MigrateThread t c) = do
    putE t
    putCap c

putEventSpec (CreateSparkThread t) =
    putE t

putEventSpec (SparkCounters crt dud ovf cnv fiz gcd rem) = do
    putE crt
    putE dud
    putE ovf
    putE cnv
    -- Warning: order of fiz and gcd reversed!
    putE gcd
    putE fiz
    putE rem

putEventSpec SparkCreate =
    return ()

putEventSpec SparkDud =
    return ()

putEventSpec SparkOverflow =
    return ()

putEventSpec SparkRun =
    return ()

putEventSpec (SparkSteal c) =
    putCap c

putEventSpec SparkFizzle =
    return ()

putEventSpec SparkGC =
    return ()

putEventSpec (WakeupThread t c) = do
    putE t
    putCap c

putEventSpec (ThreadLabel t l) = do
    putE (fromIntegral (BS.length l) + sz_tid :: EventTypeSize)
    putE t
    putByteString l

putEventSpec RequestSeqGC =
    return ()

putEventSpec RequestParGC =
    return ()

putEventSpec StartGC =
    return ()

putEventSpec GCWork =
    return ()

putEventSpec GCIdle =
    return ()

putEventSpec GCDone =
    return ()

putEventSpec EndGC =
    return ()

putEventSpec GlobalSyncGC =
    return ()

putEventSpec (TaskCreate taskId cap tid) = do
    putE taskId
    putCap cap
    putE tid

putEventSpec (TaskMigrate taskId cap new_cap) = do
    putE taskId
    putCap cap
    putCap new_cap

putEventSpec (TaskDelete taskId) =
    putE taskId

putEventSpec GCStatsGHC{..} = do
    putE heapCapset
    putE (fromIntegral gen :: Word16)
    putE copied
    putE slop
    putE frag
    putE (fromIntegral parNThreads :: Word32)
    putE parMaxCopied
    putE parTotCopied

putEventSpec HeapAllocated{..} = do
    putE heapCapset
    putE allocBytes

putEventSpec HeapSize{..} = do
    putE heapCapset
    putE sizeBytes

putEventSpec HeapLive{..} = do
    putE heapCapset
    putE liveBytes

putEventSpec HeapInfoGHC{..} = do
    putE heapCapset
    putE (fromIntegral gens :: Word16)
    putE maxHeapSize
    putE allocAreaSize
    putE mblockSize
    putE blockSize

putEventSpec CapCreate{cap} =
    putCap cap

putEventSpec CapDelete{cap} =
    putCap cap

putEventSpec CapDisable{cap} =
    putCap cap

putEventSpec CapEnable{cap} =
    putCap cap

putEventSpec (CapsetCreate cs ct) = do
    putE cs
    putE $ case ct of
            CapsetCustom -> 1 :: Word16
            CapsetOsProcess -> 2
            CapsetClockDomain -> 3
            CapsetUnknown -> 0

putEventSpec (CapsetDelete cs) =
    putE cs

putEventSpec (CapsetAssignCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (CapsetRemoveCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (RtsIdentifier cs rts) = do
    putE (fromIntegral (BS.length rts) + sz_capset :: EventTypeSize)
    putE cs
    putByteString rts

putEventSpec (ProgramArgs cs as) = do
    let as' = unsep as
    putE (fromIntegral (BS.length as') + sz_capset :: EventTypeSize)
    putE cs
    putByteString as'

putEventSpec (ProgramEnv cs es) = do
    let es' = unsep es
    putE (fromIntegral (BS.length es') + sz_capset :: EventTypeSize)
    putE cs
    putByteString es'

putEventSpec (OsProcessPid cs pid) = do
    putE cs
    putE pid

putEventSpec (OsProcessParentPid cs ppid) = do
    putE cs
    putE ppid

putEventSpec (WallClockTime cs sec nsec) = do
    putE cs
    putE sec
    putE nsec

putEventSpec (Message s) = do
    putE (fromIntegral (BS.length s) :: Word16)
    putByteString s

putEventSpec (UserMessage s) = do
    putE (fromIntegral (BS.length s) :: Word16)
    putByteString s

putEventSpec (UserMarker s) = do
    putE (fromIntegral (BS.length s) :: Word16)
    putByteString s

putEventSpec (UnknownEvent {}) = error "putEventSpec UnknownEvent"

-- [] == []
-- [x] == x\0
-- [x, y, z] == x\0y\0
unsep :: [ByteString] -> ByteString
unsep = BS.intercalate "\0"
