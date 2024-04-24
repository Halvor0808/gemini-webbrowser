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
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as B (null) 
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Applicative ((<|>), many, Applicative (liftA2))
import Control.Monad (mfilter)


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
  scheme    <- schemeParser
  authority <- pAuthority
  port      <- option 1965 (char ':' >> decimal)
  path      <- mfilter (not . B.null) pPath <|> pure "/"
  query     <- option "" (char '?' >> takeTill isEOL)
  fragment  <- option "" (char '#' >> takeTill isEOL)
  return (Url scheme authority port path query fragment)

pScheme :: Parser ByteString
pScheme = takeWhile1 (`elem` legalChars) <* "://"
    where
      legalChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-."

pAuthority :: Parser ByteString
pAuthority = Atto.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#' && c /= '\r' && c /= '\n')

pPath :: Parser ByteString
pPath = option mempty (pPath' <|> "/")
  where
    pPath' = do 
      path <- mconcat <$> many1 pSubPath
      trail <- option "/" "/"
      return $ path <> trail
    pSubPath = do
      subP <- "/" *> Atto.takeWhile1 isLegalPathChar
      return ("/"<> subP)
    isLegalPathChar c = elem c $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~!$&()+*"
