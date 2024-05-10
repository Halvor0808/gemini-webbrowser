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
import GHC.IO (unsafePerformIO)

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

getLocalPage :: FilePath -> IO [Line]
getLocalPage filePath = do
  contents <- B.readFile filePath
  case runPLines contents of
    Left err -> return [TextLine $ BSU.fromString err]
    Right response -> return response

-- | Get the help page	
-- | Even though it uses `unsafePerformIO`, it's safe because the help page is static.
-- | Until someone changes name of the file and nothing is safe about it.
getHelpPage :: [Line]
{-# NOINLINE getHelpPage #-}
getHelpPage = unsafePerformIO $ getLocalPage "app/helppage.gmi"

linesFromByteString :: BSU.ByteString -> [Line]
linesFromByteString response = do
   case runPLines response of
      Left err -> [TextLine $ BSU.fromString err <> " :\n", TextLine response]
      Right lines -> lines
