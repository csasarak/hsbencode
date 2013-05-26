-- Author: Christopher Sasarak
-- Filename: BencodeTest.hs

module Main where

import qualified Bencode as B
import qualified Text.Parsec.ByteString as BS
import qualified Text.Parsec.Error as PE
import System.Environment
import System.IO

-- This program will read a file and then print
-- its Bencoded form
main :: IO ()
main = do filename <- getArgs >>= (return . head)
          bDict <- readBencodedFile filename
          putStr $ show bDict

-- This function reads a torrent file. readTorrentFile "filename" reads
-- that filename and returns the parsed bencoded dictionary
readBencodedFile :: String -> IO (Either PE.ParseError B.Bencode)
readBencodedFile filename = BS.parseFromFile B.bDict filename
