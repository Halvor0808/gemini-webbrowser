{-# LANGUAGE OverloadedStrings #-}
module Test.Utils.ParseUtil (
    badParseTest,
    testParserIO,
) where

import Data.ByteString.Char8 hiding (putStrLn)
import Data.Attoparsec.ByteString.Char8


badParseTest :: (Show a) => Parser a -> ByteString -> IO ()
badParseTest p input = print $ feed (parse p input) empty

testParserIO :: (Show r, Eq r) => Parser r -> ByteString -> IResult ByteString r -> IO ()
testParserIO p input expected = case testParser p input expected of
    Right True  -> putStrLn "Test passed!"
    Left (msg, exp, got) -> do
        putStrLn msg
        putStrLn $ "\n" <> exp <> "\n"
        putStrLn got

testParser :: (Show r, Eq r) => Parser r -> ByteString -> IResult ByteString r -> Either (String, String, String) Bool
testParser p input expected = do
    let result =  runParser p input
    case compareParseResult result expected of
        Just True -> Right True
        _         -> Left (unpack $ "TEST FAILED ON INPUT: " <> input 
                          , "EXPECTED: " <> show expected
                          , "GOT: " <> show result)

runParser :: Parser a -> ByteString -> IResult ByteString a
runParser p input = feed (parse p input ) empty

compareParseResult :: Eq a => IResult ByteString a -> IResult ByteString a -> Maybe Bool
compareParseResult (Fail i0 _ _) (Fail i1 _ _) = if i0 == i1 then Just True else Just False
compareParseResult (Done i0 r0)  (Done i1 r1)  = if i0 == i1 && r0 == r1 then Just True else Just False
compareParseResult (Partial _)   (Partial _)   = Nothing
compareParseResult _ _                         = Just False
