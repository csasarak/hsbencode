-- Author: Christopher Sasarak
-- Filename: bencode.hs


-- This is an implementation of Bencoding for bittorrent as described at 
-- http://www.bittorrent.org/beps/bep_0003.html

module Bencode where

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Error as PE
import Data.Char
import qualified Data.Map as M

-- Technically this first argument can only be a Bstr, but I don't know
-- how to express that
type BMapT = M.Map Bencode Bencode

-- I have no idea why Bstr is okay as a String when it's a ByteString
data Bencode =  Bint Integer
              | Bstr String
              | Blist [Bencode]
              | Bmap BMapT
              deriving (Eq, Ord)

instance Show Bencode where
    show (Bint i) = "i" ++ (show i) ++ "e"
    show (Bstr s) = (show . length) s ++ ":" ++ s
    show (Blist bs) = 'l':((concat . map show) bs) ++ "e"
    show (Bmap bm) = (M.foldlWithKey (\a k b -> a ++ (show k) ++ (show b)) "d" bm)  ++ "e"

-- Parse a Bencoded Integer
bInt :: Parser Bencode
bInt = do char 'i'
          num <- validNum
          char 'e'
          return $ Bint num
       -- This parser parses valid integers in Bencodings 
       where validNum = do neg <- option ' ' (char '-')
                           d <- digit
                           case digitToInt d of
                                -- Only time the first digit == 0 is "i0e"
                                0 -> if neg == ' ' then 
                                        -- "i0e" allowed but NOT "i-0e" or zero padded integer
                                        lookAhead (char 'e') >> return 0 
                                     else
                                        parserFail "Can't have a negative zero"
                                _ -> (many digit) >>= \xs -> return $ read (neg:d:xs)
       
-- Parse a Bencoded String
bString :: Parser Bencode
bString = do ss <- many1 digit
             char ':'
             let size = read ss
             (count size $ anyChar) >>= \x -> return (Bstr x)
             
bList :: Parser Bencode
bList = do char 'l' 
           ls <- many (bInt <|> bString <|> bList <|> bMap)
           char 'e'
           return $ Blist ls
 
-- A parser which parses dictionaries 
bMap :: Parser Bencode
bMap = do char 'd'
          entries <- many dictEntry
          char 'e'
          return $ Bmap $ M.fromList entries

-- This parser will parse a key-value pair
dictEntry :: Parser (Bencode, Bencode)
dictEntry = do key <- bString
               value <- (bString <|> bList <|> bInt <|> bMap)
               return (key, value)

-- This function reads a torrent file. readBencodedFile "filename" reads
-- that filename and returns the parsed bencoded dictionary
readBencodedFile :: String -> IO (Either PE.ParseError Bencode)
readBencodedFile = parseFromFile bMap
