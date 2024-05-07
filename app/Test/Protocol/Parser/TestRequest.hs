{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestRequest (
  testRequest
)
where

import Test.Utils.ParseUtil (testParserIO)
import Protocol.Parser.Request
import Protocol.Data.Request (Url(..))
import Data.Attoparsec.ByteString (IResult(..))


testRequest :: IO ()
testRequest = do 
  putStrLn "----- Request: -----"
  testUrl
  testPath

-- They do not fail unless unaccepted 
testUrl :: IO ()
testUrl = do
  putStrLn "----- Url: -----"
  testParserIO pUrl "https://geminiprotocol.net/" True 
    (Done "" (Url "https" "geminiprotocol.net" 1965 "/" "" "")) 
-- recovers trailing "/"
  testParserIO pUrl "gemini://geminiprotocol.net" True 
    (Done "" (Url "gemini" "geminiprotocol.net" 1965 "/" "" ""))
-- recovers trailing "/"
  testParserIO pUrl "gemini://geminiprotocol.net/path/to/file" True           
    (Done "" (Url "gemini" "geminiprotocol.net" 1965 "/path/to/file/" "" ""))
-- Done w/ remainder
  testParserIO pUrl "gemini://geminiprotocol.net/errPath//" False             
    (Done "/" (Url "gemini" "geminiprotocol.net" 1965 "/errPath/" "" ""))
-- Done w/ remainder
  testParserIO pUrl "gemini://test.net/path/err//to/fail" False               
    (Done "/to/fail" (Url "gemini" "test.net" 1965 "/path/err/" "" ""))

testPath :: IO ()
testPath = do
  putStrLn "----- Path: -----"
  testParserIO pPath "/path/to/file" True  (Done "" "/path/to/file/") -- /path/to/file/
  testParserIO pPath "/path/"        True  (Done ""  "/path/")
  testParserIO pPath "/"             True  (Done "" "/")
  testParserIO pPath "//"            False (Done "/" "/")
  testParserIO pPath ""              True  (Done "" "")               -- recovers '/'  -- double check this one!!!
  testParserIO pPath "path"          False (Done "path" "")           -- no leading '/'
  testParserIO pPath "path/"         False (Done "path/" "")          -- no leading '/'
