-- Author: Christopher Sasarak
-- Filename: BencodeTest.hs

module Main where

import qualified Bencode as B
import qualified Text.Parsec.ByteString as BS
import System.Environment
import System.IO

-- This program will read a file and then print
-- its Bencoded form
main :: IO ()
main = do filename <- getArgs >>= (return . head)
          bDict <- BS.parseFromFile B.bDict filename
          putStr $ show bDict
