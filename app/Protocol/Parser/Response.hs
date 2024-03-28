{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Protocol.Parser.Response where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Control.Applicative (optional)
import Data.Text.Internal.Read (digitToInt)

import Protocol.Data.Response
import Protocol.Parser.Gemtext
import Control.Monad.State.Lazy (evalStateT)


pPacket :: Parser Packet
pPacket = do
  Packet <$> pHeader <*> pBody
  where
    pBody = evalStateT pLines False
  -- Change to execute based on what status code?

pHeader :: Parser Header
pHeader = do
  code <- pStatusCode <* char ' '
  case code of 
    (SuccessCode _ _) -> Header code <$> pMime
    _           -> return $ Header code Nothing

pMime :: Parser (Maybe MIME)
pMime = do
  typ     <- optional pMimeAlphabet
  subtype <-  char '/' *> optional pMimeAlphabet
  params  <- optional pParameters
  return $ makeMime typ subtype params

mimeAlphabet :: String
mimeAlphabet = ['a'..'z' ] ++ ['A'..'Z']
            ++ ['0'..'9']
            ++  ".-+"

isMimeAlpha :: Char -> Bool
isMimeAlpha c = c `elem` mimeAlphabet

pMimeAlphabet :: Parser B.ByteString
pMimeAlphabet = takeWhile1 isMimeAlpha


pParameters :: Parser MIMEMeta
pParameters = many1 pParam
    where
      pParam = do
        key   <- char ';' *> pMimeAlphabet -- possibly a subset, only of chars?
        value <- char '=' *> pMimeAlphabet --possibly a subset, only chars
        return (key, value)

pStatusCode :: Parser StatusCode
pStatusCode = do
  dig1 <- digitToInt <$> digit
  dig2 <- digitToInt <$> digit
  case (dig1, dig2) of
    (d1, d2) | d1 > 0 && d1 <= 6 -> return $ getStatusCode d1 d2
    _ -> fail "Invalid status code"

