{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Protocol.Parser.Request (
  pGeminiUrl,
  pUrl,
  pUrlGeneral,
  pPath,
) where

import Protocol.Data.Request hiding (fragment, query, path, port, authority, scheme)
import Utils.ParseUtil (isEOL)
import Data.Attoparsec.ByteString.Char8
    ( Parser, char, takeWhile1, decimal, isAlpha_ascii,
    isDigit, string, option, takeTill, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString (ByteString)
import Control.Applicative ((<|>), many)


pGeminiUrl :: Parser Url
pGeminiUrl = pUrlGeneral (option "gemini" "gemini" <* "://")

pUrl :: Parser Url
pUrl = pUrlGeneral pScheme

{- Stolen from ChatGPT 🥹-}
-- Argument = Parser for scheme/protocol
pUrlGeneral :: Parser ByteString -> Parser Url
pUrlGeneral schemeParser = do
  scheme <- schemeParser
  authority <- pAuthority
  port <- option 1965 (char ':' >> decimal)
  path <- pPath
  query <- option "" (char '?' >> takeTill isEOL)
  fragment <- option "" (char '#' >> takeTill isEOL)
  return (Url scheme authority port path query fragment)

pScheme :: Parser ByteString
pScheme = takeWhile1 (`elem` legalChars) <* "://"
    where
      legalChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-."

pAuthority :: Parser ByteString
pAuthority = option "" $ do
  Atto.takeWhile (\c -> c/= '/' && c /= '?' && c /= '#' && c/= '\r' && c/= '\n')

pPath :: Parser ByteString
pPath = do
   option "/" pPath'

pPath' :: Parser ByteString
pPath' = do
  path <- mconcat <$> many pSubPath
  trail <- option "/" "/"
  return $ path <> trail
  where
    isLegalPathChar c = elem c $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~!$&()+*"
    pSubPath = do
      "/"
      rest <- Atto.takeWhile1 isLegalPathChar
      return ("/"<> rest)

