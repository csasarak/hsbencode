-- Author: Christopher Sasarak
-- Filename: bencode.hs

module Bencode where

import Text.ParserCombinators.Parsec

data Bencode =  Bint Integer
              | Bstr String
              | Blist [Bencode]
              deriving (Show)




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
bDict :: Parser ()
bDict = do char 'd'
           entries <- many dictEntry
           char 'e'
           return ()

-- This parser will parse a key-value pair
dictEntry :: Parser (Bencode, Bencode)
dictEntry = do key <- bString
               value <- (bString <|> bList <|> bInt) -- TODO: Add bDict
               return (key, value)
