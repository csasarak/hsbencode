-- Author: Christopher Sasarak
-- Filename: BencodeTest.hs

module Main where

import Bencode
import System.Environment
import System.IO

-- This program will read a file and then print
-- its Bencoded form
main :: IO ()
main = do args <- getArgs
          withFile (head args) ReadMode parseBencode

--parseBencode :: Handle -> IO Bencode
parseBencode = error "Not implemented"
          
