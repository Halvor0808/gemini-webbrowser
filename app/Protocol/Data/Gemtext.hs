module Protocol.Data.Gemtext (
        Line (..),
        AltText, 
) where

import Data.ByteString

type AltText = ByteString
type LText = ByteString

data Line = TextLine             LText
          | LinkLine             LText (Maybe AltText)
          | TogglePreformatMode  AltText
          | PreformattedTextLine LText
          | HeadingLine          Int LText
          | UnorderedListLine    LText
          | QuoteLine            LText
          deriving (Show, Eq)
