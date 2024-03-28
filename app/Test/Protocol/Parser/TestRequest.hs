module Test.Protocol.Parser.TestRequest ( 
  testRequest
)
where

import Test.Utils.ParseUtil (badParseTest)
import Protocol.Parser.Request

import Data.ByteString.Char8 (pack)


testRequest :: IO ()
testRequest = do 
  badParseTest pRequest (pack "gemini://geminiprotocol.net") -- success
  badParseTest pRequest (pack "https://geminiprotocol.net") -- fail