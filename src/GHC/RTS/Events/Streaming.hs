{-# LANGUAGE LambdaCase #-}

module GHC.RTS.Events.Streaming
  ( decodeEventLog
  , decodedEvent
  ) where

import Data.ByteString.Streaming (ByteString)
import qualified Data.IntMap.Strict as IM
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes
import GHC.RTS.Events.Binary
import Streaming (Stream, Of, lift)
import Streaming.Binary
import qualified Streaming.Prelude as S

decodeEventLog
  :: Monad m
  => ByteString m r
  -> Stream (Of Event) m (Either String r)
decodeEventLog p =
    lift (decodeWith getHeader p) >>= \case
      (_, _, Left err) -> return (Left err)
      (leftover, _, Right hdr) ->
        decodedEvent hdr leftover

decodedEvent
  :: Monad m
  => Header
  -> ByteString m r
  -> Stream (Of Event) m (Either String r)
decodedEvent hdr p =
    S.concat $ decodedWith decoder0 p >>= \(_, _, res) ->
      return res
  where
    decoder0 = fmap (\ev -> ev { evCap = Nothing}) <$> getEvent (selectParsers hdr)

selectParsers :: Header -> EventParsers
selectParsers hdr =
    EventParsers $ mkEventTypeParsers imap event_parsers
  where
    imap = IM.fromList [(fromIntegral (num t), t) | t <- eventTypes hdr]
    stopParsers = [post782StopParser]
    event_parsers = concat
        [ standardParsers
        , ghc7Parsers
        , stopParsers
        ]
