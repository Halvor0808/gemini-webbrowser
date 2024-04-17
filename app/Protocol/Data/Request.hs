module Protocol.Data.Request (
    Url(..),
) where

import Data.ByteString.Char8

data Url = Url { scheme :: ByteString
               , authority :: ByteString
               , port :: Int
               , path :: ByteString
               , query :: ByteString
               , fragment :: ByteString
               } deriving  (Eq, Show)

