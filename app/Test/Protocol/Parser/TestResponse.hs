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
  print $ testParser  pResponse "15 Input prompt. Gimme some\r\n"
              (Done "" (INPUT (InputCode 1 5) "Input prompt. Gimme some"))
  print $ testParser  pResponse "1 Input prompt. Gimme some\r\n" -- missing 1 digit
              (Fail " Input prompt. Gimme some\r\n" [] "")
  print $ testParser pResponse "30 gemini://new.url.visit.to/\r\n"
              (Done "" (REDIRECT (RedirCode 3 0) (Url "gemini" "new.url.visit.to" 1965 "/" "" "")) )
  print $ testParser  pResponse "30 gemini://new.url.visit.to/" -- missing EOL
              (Fail "" [] "not enough input")
  print $ testParser pResponse "30 gemini://recover.missing.forward.slash\r\n" -- Recovers missing '/' in path
              (Done "" (REDIRECT (RedirCode 3 0) (Url "gemini" "recover.missing.forward.slash" 1965 "/" "" "")) )
  print $ testParser  pResponse "40 Error message for 40\r\n"
              (Done "" (ANY_FAIL (TempFailCode 4 0) "Error message for 40"))
  print $ testParser  pResponse "50 Error message for 50\r\n"
              (Done "" (ANY_FAIL (PermanentFailCode 5 0) "Error message for 50"))
  print $ testParser  pResponse "60 You need a ceritificate my man\r\n"
              (Done "" (ANY_FAIL (RequireCertificateCode 6 0) "You need a ceritificate my man"))
  content01 <- C8.readFile "app/Test/Input/response01-success.eg"
  print $ testParser pResponse content01 expectedResponse01
  content02 <- C8.readFile "app/Test/Input/response02-success.eg"
  print $ testParser pResponse content02 expectedResponse02
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
          Done ""  (SUCCESS {_statusCode = SuccessCode 2 0
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
                          , PreformattedTextLine {_text = "LongLineToTestBreakingLines:a123456789B123456789C123456789D123456789E123456889F123456789G123456789H123456789I123456789"}
                          , TogglePreformatMode {_ByteString = "preformat mode: off"}
                          , TextLine {_text = ""}
                          , TogglePreformatMode {_ByteString = "unicode blob"}
                          , PreformattedTextLine {_text = "     ..."}
                          , PreformattedTextLine {_text = "  ..    .."}
                          , PreformattedTextLine {_text = " ..........."}
                          , TogglePreformatMode {_ByteString = ""}
                          , QuoteLine {_text = "Quote by ME"}
                          , QuoteLine {_text = "This is hard"}
                          , QuoteLine {_text = "> Bye:)"} -- fails on this line, due to `consumeRestOfLine` function
                          ]})
