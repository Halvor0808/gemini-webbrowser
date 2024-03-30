{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol.Parser.TestGemtextParser (
    testGemtextParser
) where

import Test.Utils.ParseUtil (badParseTest)
import Protocol.Parser.Gemtext
import Control.Monad.State.Lazy (evalStateT)
import Data.ByteString (ByteString)



testGemtextParser :: IO ()
testGemtextParser = do
    putStrLn "----- GemText: -----"
    badParseTest (evalStateT pLines False) bodyEx01 -- pass
    badParseTest (evalStateT pLines False) bodyEx02 -- pass
    badParseTest (evalStateT pLines False) bodyEx03 -- pass

    where 
        bodyEx01, bodyEx02, bodyEx03 :: ByteString 
        bodyEx01 = "# Examtple title\r\nWelcome to my Gemini Capsule\n* example list tiem\n=> gemini://link.to/another/resource Linkt text replcement\r\n\r\n"
        bodyEx02 = "## Title level 2\r\nText in Line\n* A list\n* of some\n* items! \r\n => gemini://MyHub.com/more See more here!\r\n### An empty lvl 3 header\r\n"
        bodyEx03 = "### Lvl3 Head\r\nTeeeeeeeeeeeeext\nf\n\n\n\ngg\n\n\n+++"
