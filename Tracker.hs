-- Filename: Tracker.hs
-- Author: Christopher Sasarak


-- This module contains functions related to communicating with a bittorrent
-- tracker

module Tracker where

import qualified Bencode as B
import qualified Crypto.Hash.SHA1 as Crypt
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as B

-- Get a hash of this Dictionary
infoHash :: B.Bencode -> B.ByteString
infoHash = Crypt.hashlazy . BS.pack . show
