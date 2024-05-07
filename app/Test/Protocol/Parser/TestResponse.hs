{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestResponse (
   testResponse
) 
where

import Test.Utils.ParseUtil (badParseTest, testParserIO)
import Protocol.Parser.Response
import Test.Protocol.Parser.TestGemtextParser (testGemtextParser)
import Utils.ParseUtil (pParameters)

import Data.Attoparsec.ByteString.Char8 (IResult(..))
import qualified Data.ByteString.Char8 as C8
import Protocol.Data.Response (StatusCode(..), Parameters (Parameters), makeMime, Response (..), Line (..))
import Protocol.Data.Request (Url(..))
import Test.QuickCheck.State (State(expected))

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
  testParserIO pStatusCode "01" False (Fail "" [] "" )
  testParserIO pStatusCode "10" False (Done "" (InputCode   1 0))
  testParserIO pStatusCode "20" False (Done "" (SuccessCode 2 0))
  testParserIO pStatusCode "35" False (Done "" (RedirCode   3 5))
  testParserIO pStatusCode "69"  False (Done "" (RequireCertificateCode 6 9))
  testParserIO pStatusCode "70"  False (Fail "" [] "")

testParameters :: IO ()
testParameters = do
  putStrLn "----- Response Parameters: -----"
  testParserIO pResponseParams ";format=markdown\r\n" False   
        (Done "\r\n" (Parameters [("format","markdown")]))
-- spacing issue
  testParserIO pResponseParams "; notRight=meta.typing\r\n" False 
        (Fail " notRight=meta.typing\r\n" [] "")
  testParserIO pResponseParams ";typer=sub.typemega;type2=subtype2;typ3=subtyp3;mistake=sub\r\n" 
        False (Done "\r\n" (Parameters [("typer","sub.typemega"),("type2","subtype2"),("typ3","subtyp3"),("mistake","sub")]))
-- Illeagal characters
  testParserIO pResponseParams ";đu←↓→œ=wrong;\r\n" False 
        (Fail "đu←↓→œ=wrong;\r\n" [] "")
  where 
    pResponseParams = pParameters ';' '='

testMime :: IO ()
testMime = do
  putStrLn "----- Mime: -----"
  testParserIO pMime "text/gemini" True
      (Done "" (makeMime (Just ("text", "gemini")) Nothing))
  testParserIO pMime "audio/mpeg;hello=world" True
      (Done "" (makeMime (Just ("audio", "mpeg")) (Just (Parameters [("hello","world")]))))
-- spacing error -> default w/params
  testParserIO pMime "tex t/gemini;candy=nice" True 
      (mimeDefaultParams "tex t/gemini;candy=nice")
-- spacing error -> default
  testParserIO pMime "text/gemini 2;candy=nice" True 
      (mimeDefault " 2;candy=nice")
-- illegal char -> no typing or params -> defaults
  testParserIO pMime "fail/ª™§º©‘’&ŁŒıÐª" True 
      (mimeDefaultParams "fail/ª™§º©‘’&ŁŒıÐª")
  testParserIO pMime "text/gemini;format=gemtext" True
      (Done "" (makeMime (Just ("text", "gemini")) (Just (Parameters [("format","gemtext")]))))
  testParserIO pMime "text/word;format=gemtext;name=myFile" True
      (Done "" (makeMime (Just ("text", "word")) (Just (Parameters [("format","gemtext"),("name","myFile")]))) )
-- spacing error -> default
  testParserIO pMime "text/gemini;no tRight=meta.typing" True 
      (mimeDefault ";no tRight=meta.typing")
  where mimeDefault       remainder = Done remainder (makeMime (Just ("text", "gemini")) Nothing)
        mimeDefaultParams remainder = Done remainder (makeMime (Just ("text", "gemini")) (Just (Parameters [("charset","utf-8")])))

testResponseParser :: IO ()
testResponseParser = do
  putStrLn "----- Response: -----"
  testParserIO pResponse "15 Input prompt. Gimme some\r\n" True
              (Done "" (INPUT (InputCode 1 5) "Input prompt. Gimme some"))
-- missing 1 digit
  testParserIO pResponse "1 Input prompt. Gimme some\r\n" True 
              (Fail " Input prompt. Gimme some\r\n" [] "")
  testParserIO pResponse "30 gemini://new.url.visit.to/\r\n" True
              (Done "" (REDIRECT (RedirCode 3 0) (Url "gemini" "new.url.visit.to" 1965 "/" "" "")) )
-- missing EOL
  testParserIO pResponse "30 gemini://new.url.visit.to/" True 
              ( Fail "" [] "not enough input")
-- Recovers missing '/' in path
  testParserIO pResponse "30 gemini://recover.missing.forward.slash\r\n" True 
              (Done "" (REDIRECT (RedirCode 3 0) (Url "gemini" "recover.missing.forward.slash" 1965 "/" "" "")) )
  testParserIO pResponse "40 Error message for 40\r\n" True
              (Done "" (ANY_FAIL (TempFailCode 4 0) "Error message for 40"))
  testParserIO pResponse "50 Error message for 50\r\n" True
              (Done "" (ANY_FAIL (PermanentFailCode 5 0) "Error message for 50"))
  testParserIO pResponse "60 You need a ceritificate my man\r\n" True
              (Done "" (ANY_FAIL (RequireCertificateCode 6 0) "You need a ceritificate my man"))
  content01 <- C8.readFile "app/Test/Input/response01-success.eg"
  testParserIO pResponse content01 True  expectedResponse01
  content02 <- C8.readFile "app/Test/Input/response02-success.eg"
  testParserIO pResponse content02 True expectedResponse02
  where expectedResponse01 = 
          Done "" (SUCCESS {_statusCode = SuccessCode 2 0
                  , _mime = makeMime (Just ("text", "gemini")) Nothing
                  , _lines = [HeadingLine
                      {_level = 1, _text = "Example title"}
                      , TextLine {_text = "Welcome to my Gemini Capsule"}
                      , UnorderedListLine {_text = "example list tiem"}
                      , LinkLine {_link =
                                    Url {scheme = "gemini"
                                    , authority = "link.to"
                                    , port = 1965
                                    , path = "/another/resource"
                                    , query = ""
                                    , fragment = ""}
                                  , _displayText = Just "Link text replcement"}
                      , TextLine {_text = ""}]})
        expectedResponse02 = 
          Done "> Bye:)"  (SUCCESS {_statusCode = SuccessCode 2 0
                          , _mime = makeMime (Just ("text", "gemini")) (Just $ Parameters [("charset","utf-8"), ("lang","en"), ("hello","world")])
                          , _lines = [HeadingLine {_level = 1, _text = "Header1"}
                          , TextLine {_text = "Plain text line going on ... and on ..."}
                          , UnorderedListLine {_text = "item1"}
                          , UnorderedListLine {_text = "item2"}
                          , UnorderedListLine {_text = "item3"}
                          , TextLine {_text = ""}
                          , HeadingLine {_level = 2, _text = "Header 2:"}
                          , LinkLine {_link = Url {scheme = "https", authority = "rainbow.vision.on", port = 1965, path = "", query = "", fragment = ""}, _displayText = Just "Rainbow Land"}
                          , LinkLine {_link = Url {scheme = "gemini", authority = "kennedy.gemi.dev", port = 1965, path = "", query = "", fragment = ""}, _displayText = Nothing}
                          , TextLine {_text = ""}
                          , TextLine {_text = ""}
                          , HeadingLine {_level = 3, _text = "Header 3"}
                          , TogglePreformatMode {_ByteString = "preformat mode: on"}
                          , PreformattedTextLine {_text = "Preformatted text should be"}
                          , PreformattedTextLine {_text = "left to its own devices"}
                          , PreformattedTextLine {_text = "LongAssLineToTestBreakingLines:a123456789B123456789C123456789D123456789E123456889F123456789G123456789H123456789I123456789"}
                          , TogglePreformatMode {_ByteString = "preformat mode: off"}
                          , TextLine {_text = ""}
                          , TogglePreformatMode {_ByteString = "unicode blob"}
                          , PreformattedTextLine {_text = "     ..."}
                          , PreformattedTextLine {_text = "  ..    .."}
                          , PreformattedTextLine {_text = " ..........."}
                          , TogglePreformatMode {_ByteString = ""}
                          , QuoteLine {_text = "Quote by ME"}
                          , QuoteLine {_text = "This is hard"}
                       -- , QuoteLine {_text = "> Bye:)"} 
                       -- Last line is missing due to `consumeRestOfLine` which expects EOL
                          ]})
