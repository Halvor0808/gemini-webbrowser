{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Protocol.Parser.Request (
  pGeminiUrl,
  pUrl,
  pUrlGeneral,
  pPath,
) where

import Protocol.Data.Request hiding (fragment, query, path, port, authority, scheme)
import Utils.ParseUtil (isEOL, isAlphanumeric)

import Data.Attoparsec.ByteString.Lazy (Parser)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Attoparsec.ByteString.Char8 as AC (decimal)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString as BS (null) 
import Data.ByteString.Internal (c2w, ByteString)
import Control.Applicative ((<|>))
import Control.Monad (mfilter)


pGeminiUrl :: Parser Url
pGeminiUrl = pUrlGeneral scheme
  where scheme = do
          s <- "gemini" <* "://" <|> AL.takeWhile1 isAlphanumeric <* "://"
          if s /= "gemini"
            then fail "Invalid scheme: Should be \"gemini://\""
            else return s

pUrl :: Parser Url
pUrl = pUrlGeneral pScheme

pScheme :: Parser ByteString
pScheme = AL.takeWhile1 isSchemeChar <* "://"
    where
      isSchemeChar w = isAlphanumeric w ||  w `elem` map c2w "+-."

pUrlGeneral :: Parser ByteString -> Parser Url
pUrlGeneral schemeParser = do
  scheme    <- schemeParser
  authority <- pAuthority
  port      <- AL.option 1965 (":" >> AC.decimal)
  path      <- mfilter (not . BS.null) pPath <|> pure "/"
  query     <- AL.option "" ("?" >> AL.takeTill isEOL)
  fragment  <- AL.option "" ("#" >> AL.takeTill isEOL)
  return (Url scheme authority port path query fragment)

pAuthority :: Parser ByteString
pAuthority = AL.takeWhile notAuthorityChar
  where notAuthorityChar = AL.notInClass "/?#\r\n"

pPath :: Parser ByteString
pPath = AL.option mempty (pPath' <|> "/")
  where
    pPath' = do 
      path <- mconcat <$> AL.many1 pSubPath
      trail <- AL.option "/" "/"
      return $ path <> trail
    pSubPath = do
      subP <- "/" *> AL.takeWhile1 isLegalPathChar
      return ("/"<> subP)
    isLegalPathChar c = isAlphanumeric c || AL.inClass "-_.~!$&()+*" c
