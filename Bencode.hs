-- Author: Christopher Sasarak
-- Filename: bencode.hs


-- This is an implementation of Bencoding for bittorrent as described at 
-- http://www.bittorrent.org/beps/bep_0003.html

module Bencode where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M

-- Technically this first argument can only be a Bstr, but I don't know
-- how to express that
type BMapT = M.Map Bencode Bencode

-- Maybe I should use a type variable?
data Bencode =  Bint Integer
              | Bstr String
              | Blist [Bencode]
              | Bmap BMapT
              deriving (Show, Eq, Ord)


-- Parse a Bencoded Integer
bInt :: Parser Bencode
bInt = do char 'i'
          ds <- many1 digit
          char 'e'
          return $ Bint $ read ds 

-- Parse a Bencoded String
bString :: Parser Bencode
bString = do ss <- many1 digit
             char ':'
             let size = read ss
             (count size $ anyChar) >>= \x -> return (Bstr x)
             
bList :: Parser Bencode
bList = do char 'l' 
           ls <- many (bInt <|> bString <|> bList)
           char 'e'
           return $ Blist ls
 
-- A parser which parses dictionaries TODO: Make it so it does more than just
-- recognize
bDict :: Parser Bencode
bDict = do char 'd'
           entries <- many dictEntry
           char 'e'
           return $ Bmap $ M.fromList entries

-- This parser will parse a key-value pair
dictEntry :: Parser (Bencode, Bencode)
dictEntry = do key <- bString
               value <- (bString <|> bList <|> bInt) -- TODO: Add bDict
               return (key, value)
