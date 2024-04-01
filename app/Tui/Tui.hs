module Tui.Tui (tuiRun) where


import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)


data MyViewport = PageContent
  deriving (Ord, Eq, Show)

tuiRun :: IO ()
tuiRun = do 
    simpleMain ui


ui :: Widget MyViewport
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
content, contentShort, contentArea :: Widget MyViewport
contentArea = borderWithLabel (str "Content-separator")
      $ padTop (Pad 2) $ vBox [dynamicLeftRightPad 3 content, fill ' ']
content = viewport PageContent Vertical $ strWrap "Some random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo Some random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo"
contentShort = str "Short content."



{- Placeholder. TODO: make dynamnic ;_;
 -}
dynamicLeftRightPad :: Int -> Widget n -> Widget n
dynamicLeftRightPad = padLeftRight
