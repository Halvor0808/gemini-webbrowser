module Test.Utils.ParseUtil (
    badParseTest,
    testParser,
) where

import Data.ByteString
import Data.Attoparsec.ByteString.Char8


badParseTest :: (Show a) => Parser a -> ByteString -> IO ()
badParseTest p input = print $ feed (parse p input) empty

testParser :: Eq r => Parser r -> ByteString -> IResult ByteString r -> Maybe Bool
testParser p input = compareParseResult (testParser' p input)

testParser' :: Parser a -> ByteString -> IResult ByteString a
testParser' p input = feed (parse p input ) empty

compareParseResult :: Eq a => IResult ByteString a -> IResult ByteString a -> Maybe Bool
compareParseResult (Fail i0 _ _) (Fail i1 _ _) = if i0 == i1 then Just True else Nothing
compareParseResult (Partial _)   (Partial _)   = Nothing
compareParseResult (Done i0 r0)  (Done i1 r1)  = if i0 == i1 && r0 == r1 then Just True else Nothing
compareParseResult _ _                         = Just False
