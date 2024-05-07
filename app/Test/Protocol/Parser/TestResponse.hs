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
import Protocol.Data.Response (StatusCode(..), Parameters (Parameters))

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
  badParseTest pMime "text/gemini" -- works
  badParseTest pMime "audio/mpeg;hello=world" -- works
  badParseTest pMime "tex t/gemini;candy=nice" -- fails: spacing -> defaults
  badParseTest pMime "text/gem ini;candy=nice" -- fails: spacing -> text/gem (no params)
  badParseTest pMime "fail/ª™§º©‘’&ŁŒıÐª" -- fails: illegal chars -> defaults
  badParseTest pMime "text/gemini;format=gemtext" -- works
  badParseTest pMime "text/word;format=gemtext;name=myFile" -- works
  badParseTest pMime "text/spacingError;no tRight=meta.typing" -- fails: spacing -> text/spacingError (no params)


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
