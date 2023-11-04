{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.BEncode
import Data.BEncode.BDict

--- Classes

class KRPCM a where
  y :: a -> ByteString

class QueryM a where
  q :: a -> ByteString

class ResponseM a

--- KRPC Message

data (KRPCM a) => Message a = Message { t :: ByteString, content :: a }
  deriving Show
instance (KRPCM a) => KRPCM (Message a) where
  y m = y $ content m
instance (KRPCM a, QueryM a) => BEncode (Message a) where
  toBEncode m = BDict $ fromAscList [(pack "t", BString $ t m), (pack "y", BString $ y m), (pack "q", BString $ q $ content m), (pack "a", toBEncode $ content m)]
  fromBEncode = undefined

--- Messages

data (QueryM a) => Query a = Query a
  deriving Show
instance (QueryM a) => QueryM (Query a) where
  q (Query x) = q x
instance KRPCM (Query a) where
  y _ = pack "q"

data (ResponseM a) => Response a = Response a
  deriving Show
instance (ResponseM a) => ResponseM (Response a)
instance KRPCM (Response a) where
  y _ = pack "r"

data Error = Error (Integer, ByteString)
  deriving Show
instance KRPCM Error where
  y _ = pack "e"

--- Queries

data QPing = QPing
  { id :: ByteString }
  deriving Show
instance QueryM QPing where
  q _ = pack "ping"

data QFindNode = QFindNode
  { id :: ByteString
  , target :: ByteString }
  deriving Show
instance QueryM QFindNode where
  q _ = pack "find_node"

data QGetPeers = QGetPeers
  { id :: ByteString
  , info_hash :: ByteString }
  deriving Show
instance QueryM QGetPeers where
  q _ = pack "get_peers"

data QAnnouncePeer = QAnnouncePeer
  { id :: ByteString
  , info_hash :: ByteString
  , port :: Integer
  , token :: ByteString
  , implied_port :: Maybe Integer }
  deriving Show
instance QueryM QAnnouncePeer where
  q _ = pack "announce_peer"

--- Responses

data RPing = RPing
  { id :: ByteString }
  deriving Show
instance ResponseM RPing

data RFindNode = RFindNode
  { id :: ByteString
  , nodes :: ByteString }
  deriving Show
instance ResponseM RFindNode

data RGetPeersV = RGetPeersV
  { id :: ByteString
  , values :: [ByteString] }
  deriving Show
data RGetPeersN = RGetPeersN
  { id :: ByteString
  , nodes :: ByteString }
  deriving Show
instance ResponseM RGetPeersV
instance ResponseM RGetPeersN

data RAnnouncePeer = RAnnouncePeer
  { id :: ByteString }
  deriving Show
instance ResponseM RAnnouncePeer

--- Application

main :: IO ()
main = do
  let x = Message { t = (pack "aa"), content = (Query (QPing { id = (pack "abc123") })) }
  print x
  print $ encode x
  return ()
