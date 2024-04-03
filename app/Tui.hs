{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui (tuiRun) where


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


data Name = PageContent | SearchField
  deriving (Ord, Eq, Show)

data St =
  St { _focusRing :: F.FocusRing Name
     , _searchField :: E.Editor String Name
     , _content :: [Line]
     } deriving (Show)
makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [ui]
    where
      searchFieldW  = B.border $ F.withFocusRing (_focusRing st) (E.renderEditor (str . unlines)) (_searchField st)
      footer       = str "Not yet implemeted: Esc/Ctrl-q - quit, Ctrl-h - help"
      content      = viewport PageContent T.Vertical $ strWrap $ _content st
      contentArea  = B.borderWithLabel (str "Content-separator")
        $ padTop (Pad 2) $ dynamicLeftRightPad 3 content
      --
      ui =
        joinBorders $
        withBorderStyle unicode $
        B.borderWithLabel (str "GeminiBrowser") $
        dynamicLeftRightPad 3 $
        vBox [ hCenter $ padTopBottom 1 $ hLimit 30 searchFieldW <=> str "Hit Enter to search"
            , contentArea
            , hCenter footer
            ]

renderLines :: [Line] -> [Widget Name]
renderLines = map renderLine

--temp solution
-- use txtWrap/strWrap with settings instead?
renderLine :: Line -> Widget Name
renderLine (TextLine t)             = str (unpack t)
renderLine (LinkLine t _)           = str (unpack t)
renderLine (TogglePreformatMode t)  = str (unpack t)
renderLine (PreformattedTextLine t) = str (unpack t)
renderLine (HeadingLine i t)        = str (unpack t)
renderLine (UnorderedListLine t)    = str (unpack t)
renderLine (QuoteLine t)            = str (unpack t)


{- Placeholder. TODO: make dynamnic ;_;
 -}
dynamicLeftRightPad :: Int -> Widget n -> Widget n
dynamicLeftRightPad = padLeftRight

handleEvent :: T.BrickEvent Name e -> EventM Name St ()
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = do 
  M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl])) = return () -- TODO: display help
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) 
  = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) 
  = focusRing %= F.focusNext
handleEvent (T.VtyEvent (V.EvKey V.KBackTab [])) 
  = focusRing %= F.focusPrev
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of 
    Just PageContent -> handleEventPageContent ev
    Just SearchField ->  case ev of
              (T.VtyEvent (V.EvKey V.KEnter []))
                -> do 
                  sf <- use searchField
                  let query = concat $ E.getEditContents sf
                  -- TODO: Use query and network to fetch content. Also: percent-encode query
                  result <- liftIO temporaryFuncGetContentOfTestFile
                  T.modify (content .~ result)
                  return () 
              _ ->  zoom searchField $  E.handleEditorEvent ev
    Nothing -> return ()

temporaryFuncGetContentOfTestFile :: IO [Line]
-- TEMPORARY FUNCTION FOR TESTING PURPOSES: PLS REMOVE
temporaryFuncGetContentOfTestFile = do
  contents <- C8.readFile "app/Test/Input/response02-success.eg"
  case parseOnly pResponse contents of
    Left err -> return []
    Right response -> return (_lines response)

handleEventPageContent :: T.BrickEvent Name e -> EventM Name St ()
handleEventPageContent ev@(T.VtyEvent (V.EvKey key [])) = 
  case key of
    V.KUp    -> zoom content $ M.vScrollBy (M.viewportScroll PageContent) (-1)
    V.KDown  -> zoom content $ M.vScrollBy (M.viewportScroll PageContent) 1
    _        -> return ()
handleEventPageContent _ = return ()


initialState :: St
initialState =
  St (F.focusRing [SearchField, PageContent])
     (E.editor SearchField (Just 1) "")
     "Somodifye random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo Some random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowoSome random content, temporarily. wooooooooooooooooooooooooooow lots of content------------------------------------- mmmmmmmmmmmmmmmmmmmmmmm wowo"

app :: M.App St e Name
app = M.App
        { M.appDraw = drawUi
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = return () 
        , M.appAttrMap = -- Stolen example: change at convenience
            const $ attrMap
                defAttr
                  [ (attrName "selected", black `on` white) ]
        }
tuiRun :: IO St
tuiRun = do
  M.defaultMain app initialState

