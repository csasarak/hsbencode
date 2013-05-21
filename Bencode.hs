-- Author: Christopher Sasarak
-- Filename: bencode.hs

module Bencode where

import Text.ParserCombinators.Parsec

-- Parse a Bencoded Integer
integer :: Parser Integer
integer = do char 'i'
             ds <- many1 digit
             char 'e'
             return $ read ds 

-- Parse a Bencoded String
bString :: Parser String
bString = do ss <- many1 digit
             char ':'
             let size = read ss
             count size $ anyChar
             
