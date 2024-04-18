{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestRequest (
  testRequest
)
where

import Test.Utils.ParseUtil (badParseTest)
import Protocol.Parser.Request


testRequest :: IO ()
testRequest = do 
  putStrLn "----- Request: -----"
  testUrl
  testPath

testUrl :: IO ()
testUrl = do
  putStrLn "----- Url: -----"
  badParseTest pUrl "https://geminiprotocol.net/" -- success
  badParseTest pUrl "gemini://geminiprotocol.net" -- success

testPath :: IO ()
testPath = do
  putStrLn "----- Path: -----"
  badParseTest pPath "/path/to/file" -- /path/to/file/
  badParseTest pPath "/path/" -- /path/
  badParseTest pPath "/" -- /
  badParseTest pPath "//" -- / (with "/" in leftover input)
  badParseTest pPath "" -- Fails
  badParseTest pPath "path" -- Fails
  badParseTest pPath "path/" -- Fails
