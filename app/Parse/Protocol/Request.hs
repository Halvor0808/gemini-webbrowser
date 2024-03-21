{-# LANGUAGE OverloadedStrings #-}

module Parse.Protocol.Request where

import qualified Data.Attoparsec.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8 
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Char8 as C8
import Control.Applicative (optional) 

import Parse.Protocol.Response (badParseTest)

{- TODO:
 - Add Conditional handling based on protocol? (or is this strictness fine?)
 -}


{-Format
 -<URL><CR><LF>
 -}

data Request = Request { protocol :: C8.ByteString
                       --, port     :: Int 
                       , uri      :: C8.ByteString }
                       deriving (Show, Eq)
pRequest :: Parser Request
pRequest = do 
  p    <- string "gemini://"
  uri' <- takeTill (isEndOfLine . c2w)
  return $ Request p uri'


testRequest :: IO ()
testRequest = do 
  badParseTest pRequest (pack "gemini://geminiprotocol.net") -- success
  badParseTest pRequest (pack "https://geminiprotocol.net") -- fail

