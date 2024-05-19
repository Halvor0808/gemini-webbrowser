{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Protocol.Parser.Response (
  runPLines,
  pLines,
  pResponse,
  pMime,
  pStatusCode,
  pParameters,
) where

import Data.Maybe
import Data.Text.Internal.Read (digitToInt)
import Control.Applicative (optional, (<|>))
import Control.Monad.State.Lazy ( StateT, MonadState(put, get), evalStateT, MonadTrans(lift) )

import qualified Data.Attoparsec.ByteString.Lazy as AL
import Data.Attoparsec.ByteString.Lazy (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC (char, endOfLine, digit)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU ( ByteString, toString )
import qualified Data.ByteString.Char8 as BS ( ByteString)
import Network.URI

import Protocol.Data.Response
    ( MIME,
      StatusCode(..),
      Response(..),
      getStatusCode,
      makeMime,
      Line(..), Parameters (Parameters) )
import Utils.ParseUtil (pManyAlphaDigit, consumeRestOfLine, consumeRestOfLine, skipHorizontalSpace, skipWhitespace, isWhitespace, isAlphanumeric)
import Protocol.Parser.Request (pGeminiUrl)
import Protocol.Data.Request (uriToUrl)


pResponse :: Parser Response
pResponse = do
  code <- pStatusCode <* " " -- TODO
  case code of
    (InputCode   _ _) -> INPUT    code <$> consumeRestOfLine
    (SuccessCode _ _) -> SUCCESS  code <$> (pMime <* AC.endOfLine) <*> evalStateT pLines' False
    (RedirCode   _ _) -> REDIRECT code <$> pGeminiUrl <* AC.endOfLine
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
      subtype <- "/" *> pManyAlphaDigit
      return (typ, subtype)

pParameters :: Char -> Char -> Parser Parameters
pParameters separator assigner = Parameters <$> AL.many1 pParam
    where
      pParam :: Parser (BS.ByteString, BS.ByteString)
      pParam = do
          key   <- AC.char separator *> AL.takeWhile1 isValidChar
          value <- AC.char assigner  *> AL.takeWhile1 isValidChar
          return (key, value)
      isValidChar w = isAlphanumeric w || isParameterSymbol w
      isParameterSymbol = AL.inClass "-._~"

pStatusCode :: Parser StatusCode
pStatusCode = do
  dig1 <- digitToInt <$> AC.digit
  dig2 <- digitToInt <$> AC.digit
  case (dig1, dig2) of
    (d1, d2) | d1 > 0 && d1 <= 6 -> return $ getStatusCode d1 d2
    _                            -> fail "Invalid status code"

type StateParser a = StateT Bool Parser a

runPLines :: BLU.ByteString -> Either String [Line]
runPLines input = AL.eitherResult $ AL.parse (evalStateT pLines' False) input

pLines :: Parser [Line]
pLines = evalStateT pLines' False

pLines' :: StateParser [Line]
pLines' = AL.many1 pLine

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
    _ <- skipWhitespace
    url <- AL.takeTill isWhitespace
    altName <- optional (" " *> consumeRestOfLine)
    let url' = case BSU.toString url of
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
    hashes <- length <$> AL.many1 (AC.char '#') <* skipHorizontalSpace
    let maxLvl = 3
        level = min maxLvl hashes
    HeadingLine level <$> consumeRestOfLine


pUnorderedListLine :: Parser Line
pUnorderedListLine = UnorderedListLine <$> ("* " *> consumeRestOfLine)

pQuoteLine :: Parser Line
pQuoteLine = QuoteLine <$> ("> " *> consumeRestOfLine)

pTextLine :: Parser Line
pTextLine = TextLine <$> consumeRestOfLine
