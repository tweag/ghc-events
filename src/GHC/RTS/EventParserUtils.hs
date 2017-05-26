{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module GHC.RTS.EventParserUtils (
        EventParser(..),
        EventParsers(..),

        getString,
        mkEventTypeParsers,
        simpleEvent,
    ) where

import Data.Array
import Data.Binary
import Data.Binary.Get (getByteString)
import qualified Data.Binary.Get as G
import Data.Binary.Put ()
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List
import GHC.RTS.EventTypes

#define EVENTLOG_CONSTANTS_ONLY
#include <rts/EventLogFormat.h>

newtype EventParsers = EventParsers (Array Int (Get EventInfo))

getString :: EventTypeSize -> Get ByteString
getString len = getByteString (fromIntegral len)

--
-- Code to build the event parser table.
--

--
-- | Event parser data. Parsers are either fixed or vairable size.
--
data EventParser a
    = FixedSizeParser {
        fsp_type        :: Int,
        fsp_size        :: EventTypeSize,
        fsp_parser      :: Get a
    }
    | VariableSizeParser {
        vsp_type        :: Int,
        vsp_parser      :: Get a
    }

getParser :: EventParser a -> Get a
getParser (FixedSizeParser _ _ p) = p
getParser (VariableSizeParser _ p) = p

getType :: EventParser a -> Int
getType (FixedSizeParser t _ _) = t
getType (VariableSizeParser t _) = t

isFixedSize :: EventParser a -> Bool
isFixedSize (FixedSizeParser {}) = True
isFixedSize (VariableSizeParser {}) = False

simpleEvent :: Int -> a -> EventParser a
simpleEvent t p = FixedSizeParser t 0 (return p)

-- Our event log format allows new fields to be added to events over
-- time.  This means that our parser must be able to handle:
--
--  * old versions of an event, with fewer fields than expected,
--  * new versions of an event, with more fields than expected
--
-- The event log file declares the size for each event type, so we can
-- select the correct parser for the event type based on its size.  We
-- do this once after parsing the header: given the EventTypes, we build
-- an array of event parsers indexed by event type.
--
-- For each event type, we may have multiple parsers for different
-- versions of the event, indexed by size.  These are listed in the
-- eventTypeParsers list below.  For the given log file we select the
-- parser for the most recent version (largest size less than the size
-- declared in the header).  If this is a newer version of the event
-- than we understand, there may be extra bytes that we have to read
-- and discard in the parser for this event type.
--
-- Summary:
--   if size is smaller that we expect:
--     parse the earier version, or ignore the event
--   if size is just right:
--     parse it
--   if size is too big:
--     parse the bits we understand and discard the rest

mkEventTypeParsers :: IntMap EventType
                   -> [EventParser EventInfo]
                   -> Array Int (Get EventInfo)
mkEventTypeParsers etypes event_parsers
 = accumArray (flip const) undefined (0, max_event_num)
    [ (num, parser num) | num <- [0..max_event_num] ]
  where
    max_event_num = maximum (M.keys etypes)
    undeclared_etype num = fail ("undeclared event type: " ++ show num)
    parser_map = makeParserMap event_parsers
    parser num =
            -- Get the event's size from the header,
            -- the first Maybe describes whether the event was declared in the header.
            -- the second Maybe selects between variable and fixed size events.
        let mb_mb_et_size = do et <- M.lookup num etypes
                               return $ size et
            -- Find a parser for the event with the given size.
            maybe_parser mb_et_size = do possible <- M.lookup num parser_map
                                         best_parser <- case mb_et_size of
                                            Nothing -> getVariableParser possible
                                            Just et_size -> getFixedParser et_size possible
                                         return $ getParser best_parser
            in case mb_mb_et_size of
                -- This event is declared in the log file's header
                Just mb_et_size -> case maybe_parser mb_et_size of
                    -- And we have a valid parser for it.
                    Just p -> p
                    -- But we don't have a valid parser for it.
                    Nothing -> noEventTypeParser num mb_et_size
                -- This event is not declared in the log file's header
                Nothing -> undeclared_etype num

-- Find the first variable length parser.
getVariableParser :: [EventParser a] -> Maybe (EventParser a)
getVariableParser [] = Nothing
getVariableParser (x:xs) = case x of
    FixedSizeParser _ _ _ -> getVariableParser xs
    VariableSizeParser _ _ -> Just x

-- Find the best fixed size parser, that is to say, the parser for the largest
-- event that does not exceed the size of the event as declared in the log
-- file's header.
getFixedParser :: EventTypeSize -> [EventParser a] -> Maybe (EventParser a)
getFixedParser size parsers =
        do parser <- ((filter isFixedSize) `pipe`
                      (filter (\x -> (fsp_size x) <= size)) `pipe`
                      (sortBy descending_size) `pipe`
                      maybe_head) parsers
           return $ padParser size parser
    where pipe f g = g . f
          descending_size (FixedSizeParser _ s1 _) (FixedSizeParser _ s2 _) =
            compare s2 s1
          descending_size _ _ = undefined
          maybe_head [] = Nothing
          maybe_head (x:_) = Just x

padParser :: EventTypeSize -> (EventParser a) -> (EventParser a)
padParser _    (VariableSizeParser t p) = VariableSizeParser t p
padParser size (FixedSizeParser t orig_size orig_p) = FixedSizeParser t size p
    where p = if (size == orig_size)
                then orig_p
                else do d <- orig_p
                        G.skip (fromIntegral (size - orig_size))
                        return d

makeParserMap :: [EventParser a] -> IntMap [EventParser a]
makeParserMap = foldl buildParserMap M.empty
    where buildParserMap map' parser =
              M.alter (addParser parser) (getType parser) map'
          addParser p Nothing = Just [p]
          addParser p (Just ps) = Just (p:ps)

noEventTypeParser :: Int -> Maybe EventTypeSize
                  -> Get EventInfo
noEventTypeParser num mb_size = do
  bytes <- case mb_size of
             Just n  -> return n
             Nothing -> get :: Get EventTypeSize
  G.skip (fromIntegral bytes)
  return UnknownEvent{ ref = fromIntegral num }
