{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser.Request where

import Protocol.Data.Request

import Data.Attoparsec.ByteString.Char8 
import Data.ByteString.Internal (c2w)


pRequest :: Parser Request
pRequest = Request 
  <$> "gemini://" 
  <*> takeTill (isEndOfLine . c2w) -- needs optimizing?


