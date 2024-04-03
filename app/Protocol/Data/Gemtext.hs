module Protocol.Data.Gemtext (
        Line (..),
) where

import Data.ByteString

data Line = TextLine             { _text        :: ByteString }
          | TogglePreformatMode  { _ByteString  :: ByteString }
          | PreformattedTextLine { _text        :: ByteString }
          | UnorderedListLine    { _text        :: ByteString }
          | QuoteLine            { _text        :: ByteString }
          | HeadingLine          { _level       :: Int        , _text :: ByteString}
          | LinkLine             { _link        :: ByteString , _displayText :: Maybe ByteString}
          deriving (Show, Eq)
