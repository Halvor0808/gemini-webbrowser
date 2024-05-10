{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser.TestGemtextParser (
    testGemtextParser
) where

import Utils.ParseUtil (testParserIO)
import Protocol.Data.Response
import Protocol.Parser.Response
import Control.Monad.State.Lazy (evalStateT)
import Data.ByteString (ByteString)
import Protocol.Data.Request
import Data.Attoparsec.ByteString (IResult(..))



testGemtextParser :: IO ()
testGemtextParser = do
    putStrLn "----- GemText: -----"
    testParserIO (evalStateT pLines False) bodyEx01 True
        (Done "" [HeadingLine 1 "Example title"
                 , TextLine "Welcome to my Gemini Capsule"
                 , UnorderedListLine "example list item"
                 , LinkLine (Url "gemini" "link.to" 1965 "/another/resource" "" "")
                                 (Just "Link text replcement")
                 , TextLine ""
                 ])
    testParserIO (evalStateT pLines False) bodyEx02 True
        (Done "" [HeadingLine 2 "Title level 2"
                 , TextLine "Text in Line"
                 , UnorderedListLine "A list"
                 , UnorderedListLine "of some"
                 , UnorderedListLine "items!"
                 , LinkLine (Url "gemini" "MyHub.com" 1965 "/more" "" "")
                                 (Just "See more here!")
                 , HeadingLine 3 "An empty lvl 3 header"
                 ])
    testParserIO (evalStateT pLines False) bodyEx03 True  -- fails at last line: missing \r\n
        (Done "+++" [HeadingLine 3 "Lvl3 Head"
                    , TextLine "Teeeeeeeeeeeeext"
                    , TextLine "f"
                    , TextLine ""
                    , TextLine ""
                    , TextLine ""
                    , TextLine "gg"
                    , TextLine ""
                    , TextLine ""
                 -- , TextLine "+++"
                    ])

    where 
        bodyEx01, bodyEx02, bodyEx03 :: ByteString 
        bodyEx01 = "# Example title\r\n"
                <> "Welcome to my Gemini Capsule\n"
                <> "* example list item\n"
                <>"=> gemini://link.to/another/resource Link text replcement\r\n\r\n"
        bodyEx02 = "## Title level 2\r\n"
                <> "Text in Line\n"
                <> "* A list\n"
                <> "* of some\n"
                <> "* items!\r\n"
                <> "=> gemini://MyHub.com/more See more here!\r\n"
                <> "### An empty lvl 3 header\r\n"
        bodyEx03 = "### Lvl3 Head\r\n"
                <> "Teeeeeeeeeeeeext\n"
                <> "f\n\n\n\ngg\n\n\n+++"
