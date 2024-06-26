{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser.TestRequest (
  testRequest
)
where

import Utils.ParseUtil (testParserIO)
import Protocol.Parser.Request
import Protocol.Data.Request (Url(..))
import Data.Attoparsec.ByteString.Lazy (Result(..))


testRequest :: IO ()
testRequest = do 
  putStrLn "----- Request: -----"
  testUrl
  testPath

testUrl :: IO ()
testUrl = do
  putStrLn "----- Url: -----"
  simple
  recoverTrailSlash1
  recoverTrailSlash2
  simpleRemainder
  simpleRemainder2
  where
    simple             = testParserIO pUrl "https://geminiprotocol.net/"  
                            (Done "" (Url  "https" "geminiprotocol.net" 1965 "/" "" "")) 
    recoverTrailSlash1 = testParserIO pUrl "gemini://geminiprotocol.net"  
                            (Done "" (Url  "gemini" "geminiprotocol.net" 1965 "/" "" ""))
    recoverTrailSlash2 = testParserIO pUrl "gemini://geminiprotocol.net/path/to/file"            
                            (Done "" (Url  "gemini" "geminiprotocol.net" 1965 "/path/to/file/" "" ""))
    simpleRemainder    = testParserIO pUrl "gemini://geminiprotocol.net/errPath//"              
                            (Done "/" (Url "gemini" "geminiprotocol.net" 1965 "/errPath/" "" ""))
    simpleRemainder2   = testParserIO pUrl "gemini://test.net/path/err//to/fail"                
                            (Done "/to/fail" (Url "gemini" "test.net" 1965 "/path/err/" "" ""))

testPath :: IO ()
testPath = do
  putStrLn "----- Path: -----"
  testParserIO pPath "/path/to/file" (Done "" "/path/to/file/")
  testParserIO pPath "/path/"        (Done ""  "/path/")
  testParserIO pPath "/"             (Done "" "/")
  testParserIO pPath "//"            (Done "/" "/")
  testParserIO pPath ""              (Done "" "")
  testParserIO pPath "path"          (Done "path" "")
  testParserIO pPath "path/"         (Done "path/" "")
