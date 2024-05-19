{-# LANGUAGE OverloadedStrings #-}

module Utils.ParseUtil (
    isEOL,
    isWhitespace,
    isHorizontalSpace,
    isAlphanumeric,
    isAlphaChar,
    isDigit,
    consumeRestOfLine,
    skipHorizontalSpace,
    skipWhitespace,
    pManyAlphaDigit,
    testParserIO,
) where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import Data.Attoparsec.ByteString.Lazy (Result(..), Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC (endOfLine)
import qualified Data.ByteString.Lazy.UTF8 as BLU (toString)
import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)


-- Predicates
isWhitespace :: Word8 -> Bool
isWhitespace w = isHorizontalSpace w || isEOL w

isEOL :: Word8 -> Bool
isEOL w = w == c2w '\r' || w == c2w '\n'

isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == c2w ' ' || w == c2w '\t'

isAlphanumeric :: Word8 -> Bool
isAlphanumeric w = isAlphaChar w || isDigit w

-- | 'a' <= w <= 'z' || 'A' <= w <= 'Z'
isAlphaChar :: Word8 -> Bool
isAlphaChar w = w >= 65 && w <= 90 || w >= 97 && w <= 122

-- | '0' <= w <= '9'
isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57


-- Parsers
consumeRestOfLine :: Parser BS.ByteString
consumeRestOfLine = AL.takeTill isEOL <* AC.endOfLine

skipWhitespace :: Parser ()
skipWhitespace = AL.skipWhile isWhitespace

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = AL.skipWhile isHorizontalSpace

pManyAlphaDigit :: Parser BS.ByteString
pManyAlphaDigit = AL.takeWhile1 isAlphanumeric

----------------------------------------------------------------
-- Parsing Test utils
----------------------------------------------------------------

-- prints result of parse test in stdout with info if test failed.
testParserIO :: (Show r, Eq r) => Parser r -> BL.ByteString -> Result r -> IO ()
testParserIO p input expected =
    case testParser p input expected of
        Right True  -> putStrLn "Test passed!"
        Left (msg, exp, actual) -> mapM_ (print . (<> "\n"))  [msg, exp, actual]

-- returns Right True if test passed, Left with (input, expected, actual) if failed.
-- Boolean is for feeding empty input to parser, if parser requires more input to terminate.
-- i.e. avoiding `Result _` in test comparison. See `runParser` below.
testParser :: (Show r, Eq r) => Parser r -> BL.ByteString -> Result r -> Either (String, String, String) Bool
testParser p input expected = do
    let result = AL.parse p input
    case compareParseResult result expected of
        Just True -> Right True
        _         -> Left ("TEST FAILED ON INPUT: " <> BLU.toString input
                          , "EXPECTED: " <> show expected
                          , "ACTUAL  : " <> show result)

compareParseResult :: Eq a => Result a -> Result a -> Maybe Bool
compareParseResult (Fail i0 _ _) (Fail i1 _ _) = return (i0 == i1)
compareParseResult (Done i0 r0)  (Done i1 r1)  = return (i0 == i1 && r0 == r1)
compareParseResult _ _                         = Just False
