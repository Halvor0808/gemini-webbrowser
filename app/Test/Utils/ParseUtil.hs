{-# LANGUAGE OverloadedStrings #-}
module Test.Utils.ParseUtil (
    badParseTest,
    testParserIO,
) where

import Data.ByteString.Char8 hiding (putStrLn)
import Data.Attoparsec.ByteString.Char8


badParseTest :: (Show a) => Parser a -> ByteString -> IO ()
badParseTest p input = print $ feed (parse p input) empty


-- prints result of parse test in stdout with info if test failed.
testParserIO :: (Show r, Eq r) => Parser r -> ByteString -> Bool ->  IResult ByteString r -> IO ()
testParserIO p input feedEmpty expected = case testParser p input feedEmpty expected of
    Right True  -> putStrLn "Test passed!"
    Left (msg, exp, got) -> mapM_ (putStrLn . (<> "\n"))  [msg, exp, got]

-- returns Right True if test passed, Left with error message otherwise.
-- Boolean is for feeding empty input to parser, if parser requires more input to terminate.
-- i.e. avoiding `Partial _` in test comparison. See `runParser` below.
testParser :: (Show r, Eq r) => Parser r -> ByteString -> Bool ->  IResult ByteString r -> Either (String, String, String) Bool
testParser p input feedEmpty expected = do
    let result = runParser feedEmpty p input
    case compareParseResult result expected of
        Just True -> Right True
        _         -> Left (unpack $ "TEST FAILED ON INPUT: " <> input
                          , "EXPECTED: " <> show expected
                          , "GOT: " <> show result)

runParser :: Bool -> Parser a -> ByteString -> IResult ByteString a
runParser True p input  = feed (parse p input) empty
runParser False p input = parse p input

compareParseResult :: Eq a => IResult ByteString a -> IResult ByteString a -> Maybe Bool
compareParseResult (Fail i0 _ _) (Fail i1 _ _) = if i0 == i1 then Just True else Just False
compareParseResult (Done i0 r0)  (Done i1 r1)  = if i0 == i1 && r0 == r1 then Just True else Just False
compareParseResult (Partial _)   (Partial _)   = Nothing
compareParseResult _ _                         = Just False
