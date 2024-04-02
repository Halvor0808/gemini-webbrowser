{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui.Tui (tuiRun) where


import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as T
import qualified Brick.Main as M
import Brick.Widgets.Border.Style (unicode)
import Brick.AttrMap (attrMap, AttrMap, attrName)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Util (on)

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V

import Data.ByteString.Char8 (unpack)
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro

import Protocol.Data.Gemtext (Line(..))


data Name = PageContent
  deriving (Ord, Eq, Show)

tuiRun :: IO ()
tuiRun = do 
    simpleMain ui


ui :: Widget Name
ui =
    joinBorders $
    withBorderStyle unicode $ 
    borderWithLabel (str "GeminiBrowser") $ 
    dynamicLeftRightPad 3 $ 
    vBox [ searchField
         , contentArea
         , footer 
         ]

searchField, footer :: Widget n
-- replace with actual search bar
searchField = hCenter $ padTopBottom 1 $ str "search field [__________________]" 
footer = hCenter $ str "Not yet implemeted: q - quit, h - help"

-- use txtWrap instead?
content, contentShort, contentArea :: Widget Name
contentArea = borderWithLabel (str "Content-separator")
      $ padTop (Pad 2) $ vBox [dynamicLeftRightPad 3 content, fill ' ']
content = viewport PageContent Vertical $ strWrap "Some random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo Some random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo"
contentShort = str "Short content."



{- Placeholder. TODO: make dynamnic ;_;
 -}
dynamicLeftRightPad :: Int -> Widget n -> Widget n
dynamicLeftRightPad = padLeftRight
