{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestResponse (
   testResponse
) 
where

import Test.Utils.ParseUtil (badParseTest)
import Test.Protocol.Parser.TestGemtextParser (testGemtextParser)
import Protocol.Parser.Response
import Protocol.Parser.Gemtext (pLines)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Monad.State.Lazy (evalStateT)


testResponse :: IO ()
testResponse = do
  testStatusCode
  testMime
  testHeader
  testGemtextParser
  

testStatusCode :: IO ()
testStatusCode = do
  putStrLn "----- StatusCode: -----"
  badParseTest pStatusCode "01"
  badParseTest pStatusCode "10"
  badParseTest pStatusCode "20"
  badParseTest pStatusCode "35"
  badParseTest pStatusCode "69"
  badParseTest pStatusCode "70"

testParameters :: IO ()
testParameters = do
  putStrLn "----- Parameters: -----"
  badParseTest pParameters ";format=markdown"
  badParseTest pParameters "; notRight=meta.typing"
  badParseTest pParameters ";typer=sub.type+mega;type2=subtype2;typ3=subtyp3; m istake=sub"
  badParseTest pParameters ";đu←↓→œ=wrong;"

testMime :: IO ()
testMime = do
  putStrLn "----- Mime: -----"
  badParseTest pMime "text/gemini" -- works
  badParseTest pMime "audio/mpeg" -- works
  badParseTest pMime "tex t/gemini" -- fails: spacing
  badParseTest pMime "fail/ª™§º©‘’&ŁŒıÐª" -- fails: illegal chars
  badParseTest pMime "text/gemini;format=gemtext" -- works
  badParseTest pMime "text/gemini;format=gemtext;name=myFile" -- works
  badParseTest pMime "text/gemini; notRight=meta.typing" -- fails: spacing

testHeader :: IO ()
testHeader = do
  putStrLn "----- Header: -----"
  badParseTest pHeader "20 text/gemini;format=boring" -- pass
  badParseTest pHeader "20\r\n" -- fail: unexpected \r\n
  badParseTest pHeader "20 text/gemini\r\n" -- pass
  badParseTest pHeader "30 gemini://gemini.circumlunar.space\r\n" -- fail: unexpected "/"
 
getResponseEx :: IO B.ByteString
getResponseEx = C8.readFile "app/Test/Input/response.eg"
