{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Protocol.Parser.Response where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative (optional)
import Data.Text.Internal.Read (digitToInt)
import Control.Monad.State.Lazy (evalStateT)

import Protocol.Data.Response
    ( MIME,
      StatusCode(..),
      Response(..),
      getStatusCode,
      makeMime )
import Protocol.Parser.Gemtext
import Utils.ParseUtil (pParameters, pManyAlphaDigit, consumeRestOfLine)
import Protocol.Parser.Request (pUrl)


pResponse :: Parser Response
pResponse = do
  code <- pStatusCode <* char ' '
  case code of
    (InputCode   _ _) -> INPUT    code <$> consumeRestOfLine
    (SuccessCode _ _) -> SUCCESS  code <$> (pMime <* endOfLine) <*> evalStateT pLines False
    (RedirCode   _ _) -> REDIRECT code <$> pUrl <* endOfLine
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
    _ -> fail "Invalid status code"
