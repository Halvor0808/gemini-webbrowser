{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser.Gemtext
(
    runPLines, pLines
) where

import Utils.ParseUtil (consumeRestOfLine)
import Protocol.Data.Gemtext ( Line(..) )
import Data.Attoparsec.ByteString.Char8
    ( Parser, parseOnly, many1, char, isSpace, skipSpace, takeTill )
import Data.ByteString (ByteString)
import Control.Applicative (optional, (<|>))
import Control.Monad.State.Lazy
    ( StateT, MonadState(put, get), evalStateT, MonadTrans(lift) )


type StateParser a = StateT Bool Parser a

runPLines :: ByteString -> Either String [Line]
runPLines = parseOnly (evalStateT pLines False)

pLines :: StateParser [Line]
pLines = many1 pLine

pLine :: StateParser Line
pLine = do
    b <- get
    if b then lift pPreformattedTextLine
        else
            pTogglePreformatMode
            <|> lift pLinkLine
            <|> lift pHeadingLine
            <|> lift pUnorderedListLine
            <|> lift pQuoteLine
            <|> lift pTextLine

pLinkLine :: Parser Line
pLinkLine = do
    _ <- "=>"
    _ <- skipSpace
    url <- takeTill isSpace
    altName <- optional consumeRestOfLine
    return $ LinkLine url altName

pTogglePreformatMode :: StateParser Line
pTogglePreformatMode = do
    altText <- lift $ "```" *> consumeRestOfLine
    bool <- get
    put (not bool)
    return $ TogglePreformatMode altText

pPreformattedTextLine :: Parser Line
pPreformattedTextLine = PreformattedTextLine <$> consumeRestOfLine

pHeadingLine :: Parser Line
pHeadingLine = do
    hashes <- length <$> many1 (char '#') <* skipSpace 
    let level = min 3 hashes -- only between 1-3
    HeadingLine level <$> consumeRestOfLine

pUnorderedListLine :: Parser Line
pUnorderedListLine = UnorderedListLine <$> ("* " *> consumeRestOfLine)

pQuoteLine :: Parser Line
pQuoteLine = QuoteLine <$> ("> " *> consumeRestOfLine)

pTextLine :: Parser Line
pTextLine = TextLine <$> consumeRestOfLine
