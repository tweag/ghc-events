module GHC.RTS.EventTypes where
import Control.Monad

import Data.Binary

-- EventType.
type EventTypeNum = Word16
type EventTypeDescLen = Word32
type EventTypeDesc = String
type EventTypeSize = Word16
-- Event.
type EventDescription = String
type Timestamp = Word64
type ThreadId = Word32
type CapNo = Word16
type Marker = Word32
type BlockSize = Word32
type RawThreadStopStatus = Word16
type StringId = Word32
type Capset   = Word32
type TaskId = Word64
type PID = Word32

newtype KernelThreadId = KernelThreadId { kernelThreadId :: Word64 }
  deriving (Eq, Ord, Show)
instance Binary KernelThreadId where
  put (KernelThreadId tid) = put tid
  get = fmap KernelThreadId get

sz_event_type_num :: EventTypeSize
sz_event_type_num = 2
sz_cap :: EventTypeSize
sz_cap  = 2
sz_time :: EventTypeSize
sz_time = 8
sz_tid :: EventTypeSize
sz_tid  = 4
sz_capset :: EventTypeSize
sz_capset = 4
sz_capset_type :: EventTypeSize
sz_capset_type = 2
sz_block_size :: EventTypeSize
sz_block_size = 4
sz_block_event :: EventTypeSize
sz_block_event = fromIntegral (sz_event_type_num + sz_time + sz_block_size
    + sz_time + sz_cap)
sz_pid :: EventTypeSize
sz_pid = 4
sz_taskid :: EventTypeSize
sz_taskid = 8
sz_kernel_tid :: EventTypeSize
sz_kernel_tid = 8
sz_th_stop_status :: EventTypeSize
sz_th_stop_status = 2
sz_string_id :: EventTypeSize
sz_string_id = 4

{-
 - Data type delcarations to build the GHC RTS data format,
 - which is a (header, data) pair.
 -
 - Header contains EventTypes.
 - Data contains Events.
 -}
data EventLog =
  EventLog {
    header :: Header,
    dat    :: Data
  } deriving Show

newtype Header = Header {
     eventTypes :: [EventType]
  } deriving (Show, Eq)

data Data = Data {
     events :: [Event]
  } deriving Show

data EventType =
  EventType {
    num  :: EventTypeNum,
    desc :: EventTypeDesc,
    size :: Maybe EventTypeSize -- ^ 'Nothing' indicates variable size
  } deriving (Show, Eq)

data Event =
  Event {
    evTime  :: {-# UNPACK #-}!Timestamp,
    evSpec  :: EventInfo,
    evCap :: Maybe Int
  } deriving Show

data EventInfo

  -- pseudo events
  = EventBlock         { end_time   :: Timestamp,
                         cap        :: Int,
                         block_size :: BlockSize
                       }
  | UnknownEvent       { ref  :: {-# UNPACK #-}!EventTypeNum }

  -- thread scheduling
  | CreateThread       { thread :: {-# UNPACK #-}!ThreadId
                       }
  | RunThread          { thread :: {-# UNPACK #-}!ThreadId
                       }
  | StopThread         { thread :: {-# UNPACK #-}!ThreadId,
                         status :: !ThreadStopStatus
                       }
  | ThreadRunnable     { thread :: {-# UNPACK #-}!ThreadId
                       }
  | MigrateThread      { thread :: {-# UNPACK #-}!ThreadId,
                         newCap :: {-# UNPACK #-}!Int
                       }
  | WakeupThread       { thread :: {-# UNPACK #-}!ThreadId,
                         otherCap :: {-# UNPACK #-}!Int
                       }
  | ThreadLabel        { thread :: {-# UNPACK #-}!ThreadId,
                         threadlabel :: String
                       }

  -- par sparks
  | CreateSparkThread  { sparkThread :: {-# UNPACK #-}!ThreadId
                       }
  | SparkCounters      { sparksCreated, sparksDud, sparksOverflowed,
                         sparksConverted, sparksFizzled, sparksGCd,
                         sparksRemaining :: {-# UNPACK #-}! Word64
                       }
  | SparkCreate        { }
  | SparkDud           { }
  | SparkOverflow      { }
  | SparkRun           { }
  | SparkSteal         { victimCap :: {-# UNPACK #-}!Int }
  | SparkFizzle        { }
  | SparkGC            { }

  -- tasks
  | TaskCreate         { taskId :: TaskId,
                         cap :: {-# UNPACK #-}!Int,
                         tid :: {-# UNPACK #-}!KernelThreadId
                       }
  | TaskMigrate        { taskId :: TaskId,
                         cap :: {-# UNPACK #-}!Int,
                         new_cap :: {-# UNPACK #-}!Int
                       }
  | TaskDelete         { taskId :: TaskId }

  -- garbage collection
  | RequestSeqGC       { }
  | RequestParGC       { }
  | StartGC            { }
  | GCWork             { }
  | GCIdle             { }
  | GCDone             { }
  | EndGC              { }
  | GlobalSyncGC       { }
  | GCStatsGHC         { heapCapset   :: {-# UNPACK #-}!Capset
                       , gen          :: {-# UNPACK #-}!Int
                       , copied       :: {-# UNPACK #-}!Word64
                       , slop, frag   :: {-# UNPACK #-}!Word64
                       , parNThreads  :: {-# UNPACK #-}!Int
                       , parMaxCopied :: {-# UNPACK #-}!Word64
                       , parTotCopied :: {-# UNPACK #-}!Word64
                       }

  -- heap statistics
  | HeapAllocated      { heapCapset  :: {-# UNPACK #-}!Capset
                       , allocBytes  :: {-# UNPACK #-}!Word64
                       }
  | HeapSize           { heapCapset  :: {-# UNPACK #-}!Capset
                       , sizeBytes   :: {-# UNPACK #-}!Word64
                       }
  | HeapLive           { heapCapset  :: {-# UNPACK #-}!Capset
                       , liveBytes   :: {-# UNPACK #-}!Word64
                       }
  | HeapInfoGHC        { heapCapset    :: {-# UNPACK #-}!Capset
                       , gens          :: {-# UNPACK #-}!Int
                       , maxHeapSize   :: {-# UNPACK #-}!Word64
                       , allocAreaSize :: {-# UNPACK #-}!Word64
                       , mblockSize    :: {-# UNPACK #-}!Word64
                       , blockSize     :: {-# UNPACK #-}!Word64
                       }

  -- adjusting the number of capabilities on the fly
  | CapCreate          { cap :: {-# UNPACK #-}!Int
                       }
  | CapDelete          { cap :: {-# UNPACK #-}!Int
                       }
  | CapDisable         { cap :: {-# UNPACK #-}!Int
                       }
  | CapEnable          { cap :: {-# UNPACK #-}!Int
                       }

  -- capability sets
  | CapsetCreate       { capset     :: {-# UNPACK #-}!Capset
                       , capsetType :: CapsetType
                       }
  | CapsetDelete       { capset :: {-# UNPACK #-}!Capset
                       }
  | CapsetAssignCap    { capset :: {-# UNPACK #-}!Capset
                       , cap    :: {-# UNPACK #-}!Int
                       }
  | CapsetRemoveCap    { capset :: {-# UNPACK #-}!Capset
                       , cap    :: {-# UNPACK #-}!Int
                       }

  -- program/process info
  | RtsIdentifier      { capset :: {-# UNPACK #-}!Capset
                       , rtsident :: String
                       }
  | ProgramArgs        { capset :: {-# UNPACK #-}!Capset
                       , args   :: [String]
                       }
  | ProgramEnv         { capset :: {-# UNPACK #-}!Capset
                       , env    :: [String]
                       }
  | OsProcessPid       { capset :: {-# UNPACK #-}!Capset
                       , pid    :: {-# UNPACK #-}!PID
                       }
  | OsProcessParentPid { capset :: {-# UNPACK #-}!Capset
                       , ppid   :: {-# UNPACK #-}!PID
                       }
  | WallClockTime      { capset :: {-# UNPACK #-}!Capset
                       , sec    :: {-# UNPACK #-}!Word64
                       , nsec   :: {-# UNPACK #-}!Word32
                       }

  -- messages
  | Message            { msg :: String }
  | UserMessage        { msg :: String }
  | UserMarker         { markername :: String }
  deriving Show

--sync with ghc/includes/Constants.h
data ThreadStopStatus
 = NoStatus
 | HeapOverflow
 | StackOverflow
 | ThreadYielding
 | ThreadBlocked
 | ThreadFinished
 | ForeignCall
 | BlockedOnMVar
 | BlockedOnMVarRead
 | BlockedOnBlackHole
 | BlockedOnRead
 | BlockedOnWrite
 | BlockedOnDelay
 | BlockedOnSTM
 | BlockedOnDoProc
 | BlockedOnCCall
 | BlockedOnCCall_NoUnblockExc
 | BlockedOnMsgThrowTo
 | ThreadMigrating
 | BlockedOnMsgGlobalise
 | BlockedOnBlackHoleOwnedBy {-# UNPACK #-}!ThreadId
 deriving (Show)

mkStopStatus :: RawThreadStopStatus -> ThreadStopStatus
mkStopStatus n = case n of
 0  ->  NoStatus
 1  ->  HeapOverflow
 2  ->  StackOverflow
 3  ->  ThreadYielding
 4  ->  ThreadBlocked
 5  ->  ThreadFinished
 6  ->  ForeignCall
 7  ->  BlockedOnMVar
 8  ->  BlockedOnBlackHole
 9  ->  BlockedOnRead
 10 ->  BlockedOnWrite
 11 ->  BlockedOnDelay
 12 ->  BlockedOnSTM
 13 ->  BlockedOnDoProc
 14 ->  BlockedOnCCall
 15 ->  BlockedOnCCall_NoUnblockExc
 16 ->  BlockedOnMsgThrowTo
 17 ->  ThreadMigrating
 18 ->  BlockedOnMsgGlobalise
 19 ->  NoStatus -- yeuch... this one does not actually exist in GHC eventlogs
 20 ->  BlockedOnMVarRead -- since GHC-7.8.3
 _  ->  error "mkStat"

maxThreadStopStatusPre77, maxThreadStopStatus
    :: RawThreadStopStatus
maxThreadStopStatusPre77  = 18 -- see [Stop status in GHC-7.8.2]
maxThreadStopStatus = 20

data CapsetType
 = CapsetCustom
 | CapsetOsProcess
 | CapsetClockDomain
 | CapsetUnknown
 deriving Show

mkCapsetType :: Word16 -> CapsetType
mkCapsetType n = case n of
 1 -> CapsetCustom
 2 -> CapsetOsProcess
 3 -> CapsetClockDomain
 _ -> CapsetUnknown

-- | An event annotated with the Capability that generated it, if any
{-# DEPRECATED CapEvent "CapEvents will be removed soon, now Event has a field evCap" #-}
data CapEvent
  = CapEvent { ce_cap   :: Maybe Int,
               ce_event :: Event
               -- we could UNPACK ce_event, but the Event constructor
               -- might be shared, in which case we could end up
               -- increasing the space usage.
             } deriving Show

-- Checks if the capability is not -1 (which indicates a global eventblock), so
-- has no associated capability
mkCap :: Int -> Maybe Int
mkCap cap = do
  guard $ fromIntegral cap /= (maxBound :: Word16)
  return cap
