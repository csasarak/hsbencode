
module Main where

import Tracker
import qualified Bencode as B
import System.Environment

-- main test program
main :: IO ()
main = do args <- getArgs
          Right m <- B.readBencodedFile $ head args
          trackerGET m
