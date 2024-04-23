{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Protocol.Parser.Request (
  pGeminiUrl,
  pUrl,
  pUrlGeneral,
  pPath,
) where

import Protocol.Data.Request hiding (fragment, query, path, port, authority, scheme)
import Utils.ParseUtil (isEOL, isAlphaDigit)
import Data.Attoparsec.ByteString.Char8
    ( Parser, char, takeWhile1, decimal, isAlpha_ascii,
    isDigit, string, option, takeTill, endOfLine, satisfy, many1)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Applicative ((<|>), many)


pGeminiUrl :: Parser Url
pGeminiUrl = pUrlGeneral scheme
  where scheme = do
          s <- "gemini" <* "://" <|> takeWhile1 isAlphaDigit <* "://"
          if s /= "gemini"
            then fail "Invalid scheme: Should be \"gemini://\"" 
            else return s


pUrl :: Parser Url
pUrl = pUrlGeneral pScheme

{- Stolen from ChatGPT 🥹-}
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
pAuthority = Atto.takeWhile (\c -> c/= '/' && c /= '?' && c /= '#' && c/= '\r' && c/= '\n')

pPath :: Parser ByteString
pPath = option "/" pPath'
  where
    isLegalPathChar c = elem c $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~!$&()+*"
    pPath' :: Parser ByteString
    pPath' = do
      (mconcat <$> many pSubPath) <> option "/" "/"
    pSubPath = do
      "/"
      rest <- Atto.takeWhile1 isLegalPathChar
      return ("/"<> rest)

