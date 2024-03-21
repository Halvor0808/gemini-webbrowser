module Protocol.Data.Request where

import Data.ByteString.Char8

{-Format
 -<URL><CR><LF>
 -}
data Request = Request { protocol :: ByteString
                       --, port     :: Int 
                       , uri      :: ByteString }
                       deriving (Show, Eq)