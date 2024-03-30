module Protocol.Data.Request (
    Url(..),
    Request,
) where

import Data.ByteString.Char8

type Request = Url
data Url = Url { scheme :: ByteString
               , authority :: ByteString
               , port :: Int
               , path :: ByteString
               , query :: ByteString
               , fragment :: ByteString
               } deriving  (Eq, Show)

