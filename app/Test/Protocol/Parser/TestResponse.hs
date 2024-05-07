{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestResponse (
   testResponse
) 
where

import Test.Utils.ParseUtil (badParseTest, testParser)
import Protocol.Parser.Response
import Test.Protocol.Parser.TestGemtextParser (testGemtextParser)
import Utils.ParseUtil (pParameters)

import Data.Attoparsec.ByteString.Char8 (IResult(..))
import qualified Data.ByteString.Char8 as C8
import Protocol.Data.Response (StatusCode(..), Parameters (Parameters), makeMime)

testResponse :: IO ()
testResponse = do
  testStatusCode
  testParameters
  testMime
  testGemtextParser
  testResponseParser

testStatusCode :: IO ()
testStatusCode = do
  putStrLn "----- StatusCode: -----"
  print $ testParser pStatusCode "01" (Fail "" [] "" )
  print $ testParser pStatusCode "10" (Done "" (InputCode   1 0))
  print $ testParser pStatusCode "20" (Done "" (SuccessCode 2 0))
  print $ testParser pStatusCode "35" (Done "" (RedirCode   3 5))
  print $ testParser pStatusCode "69" (Done "" (RequireCertificateCode 6 9))
  print $ testParser pStatusCode "70" (Fail "" [] "")

testParameters :: IO ()
testParameters = do
  putStrLn "----- Response Parameters: -----"
  print $ testParser pResponseParams ";format=markdown\r\n"       
                     (Done "\r\n" (Parameters [("format","markdown")]))
  print $ testParser pResponseParams "; notRight=meta.typing\r\n" 
                     (Fail " notRight=meta.typing\r\n" [] "")
  print $ testParser pResponseParams ";typer=sub.typemega;type2=subtype2;typ3=subtyp3;mistake=sub\r\n"
                     (Done "\r\n" (Parameters [("typer","sub.typemega"),("type2","subtype2"),("typ3","subtyp3"),("mistake","sub")]))
  print $ testParser pResponseParams ";đu←↓→œ=wrong;\r\n"
                     (Fail "đu←↓→œ=wrong;\r\n" [] "")
  where 
    pResponseParams = pParameters ';' '='

testMime :: IO ()
testMime = do
  putStrLn "----- Mime: -----"
  print $ testParser pMime "text/gemini"
      (Done "" (makeMime (Just ("text", "gemini")) Nothing))
  print $ testParser pMime "audio/mpeg;hello=world"
      (Done "" (makeMime (Just ("audio", "mpeg")) (Just (Parameters [("hello","world")]))))
  print $ testParser pMime "tex t/gemini;candy=nice"  -- spacing error -> default
      (mimeDefaultParams "tex t/gemini;candy=nice")
  print $ testParser pMime "text/gemini 2;candy=nice" -- spacing error -> text/gem (no params)
      (mimeDefault " 2;candy=nice")
  print $ testParser pMime "fail/ª™§º©‘’&ŁŒıÐª" -- illegal char -> defaults
      (mimeDefaultParams "fail/ª™§º©‘’&ŁŒıÐª")
  print $ testParser pMime "text/gemini;format=gemtext"
      (Done "" (makeMime (Just ("text", "gemini")) (Just (Parameters [("format","gemtext")]))))
  print $ testParser pMime "text/word;format=gemtext;name=myFile"
      (Done "" (makeMime (Just ("text", "word")) (Just (Parameters [("format","gemtext"),("name","myFile")]))) )
  print $ testParser pMime "text/gemini;no tRight=meta.typing" -- spacing error -> text/gemini (no params)
      (mimeDefault ";no tRight=meta.typing")
  where mimeDefault        remainder = Done remainder (makeMime (Just ("text", "gemini")) Nothing)
        mimeDefaultParams remainder = Done remainder (makeMime (Just ("text", "gemini")) (Just (Parameters [("charset","utf-8")])))

testResponseParser :: IO ()
testResponseParser = do
  putStrLn "----- Response: -----"
  badParseTest pResponse "15 Input prompt. Gimme some\r\n" -- works
  badParseTest pResponse "1 Input prompt. Gimme some\r\n" -- Fails: 1 digit
  C8.readFile "app/Test/Input/response01-success.eg" >>= badParseTest pResponse -- works
  C8.readFile "app/Test/Input/response02-success.eg" >>= badParseTest pResponse -- works
  badParseTest pResponse "30 gemini://new.url.visit.to/\r\n" -- works
  badParseTest pResponse "30 gemini://new.url.visit.to/" -- Fails: missing EOL
  badParseTest pResponse "30 gemini://missing.forward.slash\r\n" -- works: Recovers missing /
  badParseTest pResponse "40 Error message for 40\r\n" -- works
  badParseTest pResponse "50 Error message for 50\r\n" -- works
  badParseTest pResponse "60 You need a ceritificate my man\r\n" -- works
