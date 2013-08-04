-- Filename: Tracker.hs
-- Author: Christopher Sasarak

-- This module contains functions related to communicating with a bittorrent
-- tracker
module Tracker where

import qualified Bencode as B
import qualified Network.Curl as Cu
import qualified Crypto.Hash.SHA1 as Crypt
import qualified Data.ByteString.Lazy.Char8 as LB 
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.List as DL
import Data.Word
import Network.BSD
import qualified Network.URI as URI
import Debug.Trace


-- Get a hash of this Dictionary
infoHash :: B.Bencode -> BS.ByteString
infoHash = Crypt.hashlazy . LB.pack . show

-- A GET request with the given bdict as an info file to the tracker
trackerGET :: B.Bencode -> IO ()
trackerGET m = do hostInfo <- getHostByName "localhost"
                  let url = genTrackerURL (head . hostAddresses $ hostInfo) m 
                  trace (show url) $ Cu.curlGet url [] 

-- Send a trackerGet with the given filename as the Bencoded dictionary to read from
sendTrackerGET :: String -> IO ()
sendTrackerGET s = do Right m <- B.readBencodedFile s
                      trackerGET m

-- This will take a Bencoded dictionary i.e. from a .torrent file and
-- produce a URL suitable for use in a tracker GET request
genTrackerURL :: Word32 -> B.Bencode -> String
genTrackerURL ip (B.Bmap m) = tAddr ++  "?" ++ (DL.intercalate "," .  map (\(x, y) ->  x ++ "=" ++ y)) urlKeyVals
               where info_hash = infoHash $ m Map.! B.Bstr "info"
                     B.Bstr tAddr = m Map.! B.Bstr "announce"
                     port = 0 -- TODO
                     uploaded = 0
                     downloaded = 0
                     left = 0
                     urlKeyVals = [ ("info_hash", show info_hash), ("ip", show ip),
                        ("uploaded", show uploaded), ("downloaded", show downloaded),
                        ("left", show left)]

                     
                     
