{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.Protocol.Parser.TestResponse -- (tests) 
where

import Test.Utils.ParseUtil (badParseTest)
import Protocol.Parser.Response

import qualified Data.ByteString as B
-- import Data.ByteString.Char8 (pack, unpack, readFile)
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString (takeByteString)


tests :: IO ()
tests = do
  testStatusCode
  testCrlf
  testMime
  testHeader
  testBody

testStatusCode :: IO ()
testStatusCode = do
  putStrLn "----- StatusCode: -----"
  badParseTest pStatusCode "01\r\n"
  badParseTest pStatusCode "20\r\n"
  badParseTest pStatusCode "35\r\n"
  badParseTest pStatusCode "69\r\n"
  badParseTest pStatusCode "70\r\n"

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


testCrlf :: IO ()
testCrlf = do
  putStrLn "----- Crlf: -----"
  badParseTest pCrlf crlf
  badParseTest pCrlf "\n\rnope"
  badParseTest pCrlf "\n\r "

testHeader :: IO ()
testHeader = do
  putStrLn "----- Header: -----"
  badParseTest pHeader "20\r\n text/gemini;format=boring"
  badParseTest pHeader header01
  badParseTest pHeader header02
  badParseTest pHeader header03
  -- Add other test cases

header01, header02, header03 :: B.ByteString
header01 = "20\r\n"
header02 = "20 text/gemini\r\n"
header03 = "30 gemini://gemini.circumlunar.space\r\n"

testBody :: IO ()
testBody = do
  putStrLn "----- Body: -----"
  getResponseEx >>= badParseTest takeByteString

getResponseEx :: IO B.ByteString
getResponseEx = C8.readFile "app/Test/Input/response.eg"
