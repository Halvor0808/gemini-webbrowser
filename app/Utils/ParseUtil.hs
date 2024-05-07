{-# LANGUAGE OverloadedStrings #-}

module Utils.ParseUtil (
    consumeRestOfLine,
    pParameters,
    pManyAlphaDigit,
    isEOL,
    skipHorizontalSpace,
    isAlphaDigit,
    testParserIO,
) where

import Protocol.Data.Response (Parameters(..))
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 hiding (elem, putStrLn)

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
pManyAlphaDigit = takeWhile1 isAlphaDigit

isAlphaDigit :: Char -> Bool
isAlphaDigit c = c `elem` (alpha ++ nums)

alpha, nums :: [Char]
alpha = ['a'..'z' ] ++ ['A'..'Z']
nums =  ['0'..'9']

----------------------------------------------------------------
-- Parsing Test utils
----------------------------------------------------------------

-- prints result of parse test in stdout with info if test failed.
testParserIO :: (Show r, Eq r) => Parser r -> ByteString -> Bool ->  IResult ByteString r -> IO ()
testParserIO p input feedEmpty expected = case testParser p input feedEmpty expected of
    Right True  -> putStrLn "Test passed!"
    Left (msg, exp, actual) -> mapM_ (putStrLn . (<> "\n"))  [msg, exp, actual]

-- returns Right True if test passed, Left with (input, expected, actual) if failed.
-- Boolean is for feeding empty input to parser, if parser requires more input to terminate.
-- i.e. avoiding `Partial _` in test comparison. See `runParser` below.
testParser :: (Show r, Eq r) => Parser r -> ByteString -> Bool ->  IResult ByteString r -> Either (String, String, String) Bool
testParser p input feedEmpty expected = do
    let result = runParser feedEmpty p input
    case compareParseResult result expected of
        Just True -> Right True
        _         -> Left (unpack $ "TEST FAILED ON INPUT: " <> input
                          , "EXPECTED: " <> show expected
                          , "ACTUAL  : " <> show result)

runParser :: Bool -> Parser a -> ByteString -> IResult ByteString a
runParser True p input  = feed (parse p input) empty
runParser False p input = parse p input

compareParseResult :: Eq a => IResult ByteString a -> IResult ByteString a -> Maybe Bool
compareParseResult (Fail i0 _ _) (Fail i1 _ _) = if i0 == i1 then Just True else Just False
compareParseResult (Done i0 r0)  (Done i1 r1)  = if i0 == i1 && r0 == r1 then Just True else Just False
compareParseResult (Partial _)   (Partial _)   = Nothing
compareParseResult _ _                         = Just False
