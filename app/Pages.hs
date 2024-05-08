{-# LANGUAGE OverloadedStrings #-}

module Pages (
    getHomePage,
    getTestPage,
    getHelpPage,
    home,
    linesFromByteString,
) where


import Protocol.Parser.Response (runPLines)
import Protocol.Data.Response (Line(..), Response(..))
import Protocol.Data.Request (Url(..))

import Data.ByteString.UTF8 as BSU
import Data.ByteString as B (readFile)

getTestPage :: IO [Line]
getTestPage = do
 getLocalPage "app/Test/Input/response02-success.eg"

getHomePage :: IO [Line]
getHomePage = getLocalPage "app/homepage.gmi"

home :: Url
home = Url { scheme = ""
           , authority = ""
           , port = 1965
           , path = "home"
           , query = ""
           , fragment = ""
           }

-- This is dreadful, but I am not sure if it's possible to to do this without IO.
-- feeding readFile to a Widget does not work in the `DrawUI` function. Cannot liftIO, or sim.
-- `fileContent :: IO [Line]` fed into `drawUI :: St -> [Widget Name]`.
getHelpPage :: BSU.ByteString
getHelpPage = BSU.fromString $ unlines [
 "# Gemini Web Browser -- Help Page "
 , " "
 , "There are 3 main screens of the application."
 , " "
 , "1 The Browser"
 , "* It has 2 main sub-components. Only one can be in focus at a time. Press the <TAB> button to toggle between them."
 , "* Search field        --> Enter urls to request pages. \"home\" for the home-screen. Press <ENTER>." 
 , "* Content display     --> See rendered Gemtext content of requested page."
 , " "
 , "2 The Help Page (Where you are now!)"
 , " "
 , "3 The History Page"
 , "* A chronological stack of visited pages. No fancy handling. All pages are added, even if the request fails."
 , "* You can revisit pages in the stack by pressing <ENTER>."
 , " "
 , "## Controls"
 , " "
 , "Mouse Currently not supported"
 , " "
 , "### Keyboard Bindings"
 , " "
 , "> Ctrl+q / Esc        = Exit application."
 , "> Tab                 = Toggle between which component is in focus. (Search-field or Content-display)."
 , "> Ctrl+w              = Show current page in browser."
 , "> Ctrl+e              = Toggle Help menu."
 , "> Ctrl+r              = Toggle History menu."
 , "> Arrow key UP        = Scroll content UP."
 , "> Arrow key DOWN      = Scroll content DOWN."
 , "> <Enter> button      = When cursor on URL, navigate to URL."
 , " "
 , "## Thanks!"
 , "Thank you for trying out my Haskell project!"
 , "Sincerely, Halvor Brunt."
 , "Code is available (in your regular http browser) at:"
 , "=> https://github.com/Halvor0808/gemini-webbrowser"
 ]

getLocalPage :: FilePath -> IO [Line]
getLocalPage filePath = do
  contents <- B.readFile filePath 
  case runPLines contents of
    Left err -> return [TextLine $ BSU.fromString err]
    Right response -> return response

linesFromByteString :: BSU.ByteString -> [Line]
linesFromByteString response = do
   case runPLines response of
      Left err -> [TextLine $ BSU.fromString err <> " :\n", TextLine response]
      Right lines -> lines
