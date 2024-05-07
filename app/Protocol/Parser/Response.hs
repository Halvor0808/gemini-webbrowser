{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Protocol.Parser.Response (
  runPLines,
  pLines,
  pResponse,
  pMime,
  pStatusCode,
) where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative (optional, (<|>))
import Data.Text.Internal.Read (digitToInt)
import Control.Monad.State.Lazy ( StateT, MonadState(put, get), evalStateT, MonadTrans(lift) )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

import Protocol.Data.Response
    ( MIME,
      StatusCode(..),
      Response(..),
      getStatusCode,
      makeMime,
      Line(..) )
import Utils.ParseUtil (pParameters, pManyAlphaDigit, consumeRestOfLine, consumeRestOfLine, skipHorizontalSpace)
import Protocol.Parser.Request (pGeminiUrl)
import Protocol.Data.Request (uriToUrl)
import Network.URI
import Data.Maybe


pResponse :: Parser Response
pResponse = do
  code <- pStatusCode <* char ' '
  case code of
    (InputCode   _ _) -> INPUT    code <$> consumeRestOfLine
    (SuccessCode _ _) -> SUCCESS  code <$> (pMime <* endOfLine) <*> evalStateT pLines False
    (RedirCode   _ _) -> REDIRECT code <$> pGeminiUrl <* endOfLine
    _                 -> ANY_FAIL code <$> consumeRestOfLine -- common case for fails & cetrificate requests
--    (TempFailCode _ _) -> TEMP_FAIL code <$> consumeRestOfLine
--    (PermanenetFailCode _ _) -> PERM_FAIL code <$> consumeRestOfLine
--    (RequireCertificateCode _ _) -> CLIENT_CERT code <$> consumeRestOfLine

pMime :: Parser (Maybe MIME)
pMime = do
  typing <- optional pType
  params  <- optional (pParameters ';' '=')
  return $ makeMime typing params
  where
    pType = do
      typ     <- pManyAlphaDigit
      subtype <- char '/' *> pManyAlphaDigit
      return (typ, subtype)


pStatusCode :: Parser StatusCode
pStatusCode = do
  dig1 <- digitToInt <$> digit
  dig2 <- digitToInt <$> digit
  case (dig1, dig2) of
    (d1, d2) | d1 > 0 && d1 <= 6 -> return $ getStatusCode d1 d2
    _                            -> fail "Invalid status code"

type StateParser a = StateT Bool Parser a

runPLines :: ByteString -> Either String [Line]
runPLines i = eitherResult . (`feed` "") . (`feed` "\n") $ parse (evalStateT pLines False) i

pLines :: StateParser [Line]
pLines = many1 pLine

pLine :: StateParser Line
pLine = do
    b <- get
    if b then
            pTogglePreformatMode
            <|> lift pPreformattedTextLine
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
    altName <- optional (" " *> consumeRestOfLine)
    let url' = case unpack url of
            ('/':xs) -> uriToUrl $ nullURI {uriPath = '/':xs}
            u        -> uriToUrl $ fromMaybe (error "invalid url") (parseURIReference u)
    return $ LinkLine { _link = url', _displayText = altName }

pTogglePreformatMode :: StateParser Line
pTogglePreformatMode = do
    altText <- lift $ "```" *> skipHorizontalSpace *> consumeRestOfLine
    bool <- get
    put (not bool)
    return $ TogglePreformatMode altText

pPreformattedTextLine :: Parser Line
pPreformattedTextLine = PreformattedTextLine <$> consumeRestOfLine

pHeadingLine :: Parser Line
pHeadingLine = do
    hashes <- length <$> many1 (char '#') <* skipSpace
    let maxLvl = 3
        level = min maxLvl hashes
    HeadingLine level <$> consumeRestOfLine


pUnorderedListLine :: Parser Line
pUnorderedListLine = UnorderedListLine <$> ("* " *> consumeRestOfLine)

pQuoteLine :: Parser Line
pQuoteLine = QuoteLine <$> ("> " *> consumeRestOfLine)

pTextLine :: Parser Line
pTextLine = TextLine <$> consumeRestOfLine
