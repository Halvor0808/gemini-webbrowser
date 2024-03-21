module Test.Utils.ParseUtil (
    badParseTest,
    testPipe,
) where

import Data.ByteString
import Data.Attoparsec.ByteString.Char8


badParseTest :: (Show a) => Parser a -> ByteString -> IO ()
badParseTest p input = print $ feed (parse p input) empty



-- possible replacement for badParseTest?
testPipe :: Parser a -> ByteString -> Either String a
testPipe parser input = eitherResult $ feed (parse parser input) empty

testPipe' :: (Show a) => Parser a -> ByteString -> IO ()
testPipe' parser input = print $ testPipe parser input
