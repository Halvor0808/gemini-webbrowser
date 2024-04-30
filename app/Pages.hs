{-# LANGUAGE OverloadedStrings #-}

module Pages (
    getHomePage,
    getTestPage,
    getHelpPage,
) where


import Protocol.Parser.Response (runPLines)
import Protocol.Data.Response (Line(..), Response(..))

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString as B (readFile)

getTestPage :: IO [Line]
getTestPage = do
 getLocalWebPage "app/Test/Input/response02-success.eg"

getHomePage :: IO [Line]
getHomePage = getLocalWebPage "app/homepage.gmi"

getHelpPage :: String
getHelpPage = unlines ["# Gemini Web Browser -- Help Page "
                      , "Controls:"
                      ,  ""
                      , "There are keyboard button controls, and mouse controls."
                      , "The keyboard can request a page by URL, and navigate a page with the arrow keys."
                      , "The mouse can do the same but with the mouse wheel."
                      , ""
                      , "## Bindings"
                      , "Ctrl+q / Esc (in main menu) = Exit application"
                      , "Esc                         = Exit window"
                      , "Ctrl+e                      = open/close help menu"
                      , "Arrow key UP                = Scroll content UP"
                      , "Arrow key DOWN              = Scroll content DOWN"]



getLocalWebPage :: FilePath -> IO [Line]
getLocalWebPage filePath = do
  contents <- B.readFile filePath 
  case runPLines contents of
    Left err -> return [TextLine $ pack err]
    Right response -> return response
