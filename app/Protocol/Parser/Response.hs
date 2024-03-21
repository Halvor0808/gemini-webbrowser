{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Protocol.Parser.Response where

import qualified Data.Attoparsec.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Control.Applicative (optional) --(<|>), many

import Protocol.Data.Response
import Data.Text.Internal.Read (digitToInt)

pPacket :: Parser Packet
pPacket = do
  header <- pHeader
  Packet header <$> pBody
  where
    pBody = takeByteString  -- bad implementation, should limit on length(??)
  -- Change to execute based on what status code?



pHeader :: Parser Header
pHeader = do
  status <- pStatusCode
  _      <- char ' '
  mime   <- optional pMime -- optional?
  return $ Header status mime


--------------------------------------
-- unused?
makeMime :: Maybe MainMimeType -> Maybe SubMimeType -> Maybe MIMEMeta -> Maybe MIME
makeMime Nothing _ _                = Nothing
makeMime _ Nothing _                = Nothing
makeMime (Just typ) (Just subtyp) p = Just $ MIME typ subtyp p
---------------------------------

pMime :: Parser MIME
pMime = do
  typ     <- pMimeAlphabet
  _       <- char '/'
  subtype <- pMimeAlphabet
  params  <- optional pParameters -- optional?
  return $ MIME typ subtype params

mimeAlphabet :: String
mimeAlphabet = ['a'..'z' ] ++ ['A'..'Z']
            ++ ['0'..'9']
            ++  ".-+"

isMimeAlpha :: Char -> Bool
isMimeAlpha c = c `elem` mimeAlphabet

pMimeAlphabet :: Parser B.ByteString
pMimeAlphabet = BC.takeWhile1 isMimeAlpha


pParameters :: Parser MIMEMeta
pParameters = many1 pParam
    where
      pParam = do
        key   <- char ';' *> pMimeAlphabet -- possibly a subset, only of chars?
        value <- char '=' *> pMimeAlphabet --possibly a subset, only chars
        return (key, value)

pCrlf :: Parser ()
pCrlf = do
  _ <- "\r\n"
  return ()

pStatusCode :: Parser StatusCode
pStatusCode = do
  dig1 <- digitToInt <$> digit :: Parser Int
  dig2 <- digitToInt <$> (digit <* endOfLine)
  case (dig1, dig2) of
    (d1, d2) | d1 > 0 && d1 <= 6 -> return $ getStatusCode d1 d2
    _ -> fail "Invalid status code"



