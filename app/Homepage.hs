module Homepage (
    getHomePage
) where

import Protocol.Data.Response (Response(..))
import Protocol.Parser.Gemtext (runPLines)
import Protocol.Data.Gemtext (Line(..))

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString as B (readFile)


getHomePage :: IO [Line]
getHomePage = getLocalWebPage "app/homepage.gmi"

getLocalWebPage :: FilePath -> IO [Line]
getLocalWebPage filePath = do
  contents <- B.readFile filePath 
  case runPLines contents of
    Left err -> return [TextLine $ pack err]
    Right response -> return response
