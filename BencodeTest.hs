{-|
  Module      : BencodeTest
  Description : Test program for Bencode
  Copyright   : (c) Christopher Sasarak, 2014
  License     : GPL-3
  Maintainer  : cms5347@rit.edu
  Stability   : experimental
  
 -}
module BencodeTest where

import qualified Bencode as B
import qualified Text.Parsec.ByteString as BS
import qualified Text.Parsec.Error as PE
import System.Environment
import System.IO

-- |This program will read a file and then print
-- its Bencoded form as a String
main :: IO ()
main = do filename <- getArgs >>= (return . head)
          bDict <- B.readBencodedFile filename
          putStr $ show bDict
