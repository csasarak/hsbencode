
module Main where

import Tracker
import qualified Bencode as B
import System.Environment

-- main test program
main :: IO ()
main = do args <- getArgs
          sendTrackerGET . head $ args 
