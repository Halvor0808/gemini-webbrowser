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
               } deriving  (Eq)

instance Show Url where
    show (Url scheme authority port path query fragment) = 
        show scheme ++ "://" ++ show authority ++ ":" ++ show port ++ show path ++ show query ++ show fragment

