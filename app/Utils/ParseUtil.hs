{-# LANGUAGE OverloadedStrings #-}

module Utils.ParseUtil (
    consumeRestOfLine,
    pParameters,
    pManyAlphaDigit,
    isEOL,
    skipHorizontalSpace,
) where

import Data.Attoparsec.ByteString.Char8  
  (Parser, takeTill, takeWhile1, endOfLine, char, many1, skipWhile)
import Data.ByteString (ByteString)
import Protocol.Data.Response (Parameters(..))


consumeRestOfLine :: Parser ByteString
consumeRestOfLine = takeTill isEOL <* endOfLine

isEOL :: Char -> Bool
isEOL c = c == '\n' || c == '\r'

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (`elem` [' ', '\t'])

pParameters :: Char -> Char -> Parser Parameters
pParameters separator assigner = Parameters <$> many1 pParam
    where
      pParam :: Parser (ByteString, ByteString)
      pParam = do
          key   <- char separator *> takeWhile1 isParameterChar
          value <- char assigner  *> takeWhile1 isParameterChar
          return (key, value)
      isParameterChar = (`elem` (alpha ++ nums ++ "-._~"))

pManyAlphaDigit :: Parser ByteString
pManyAlphaDigit = takeWhile1 isSimpleAlphabet

isSimpleAlphabet :: Char -> Bool
isSimpleAlphabet c = c `elem` (alpha ++ nums)

alpha, nums :: [Char]
alpha = ['a'..'z' ] ++ ['A'..'Z']
nums =  ['0'..'9']