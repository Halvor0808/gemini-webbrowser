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
import Brick.Util (on, fg)

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Txt
import Text.Wrap
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro

import Protocol.Data.Gemtext (Line(..))
import Protocol.Parser.Response (pResponse)
import qualified Data.ByteString.Char8 as C8
import Protocol.Data.Response


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
      searchField  = B.border $ F.withFocusRing (_focusRing st)
                                (E.renderEditor (str . unlines)) (_searchField st)
      contentArea  = B.borderWithLabel (str "Content-separator")
                     $ padTop (Pad 2) $ dynamicLeftRightPad 3
                     $ viewport PageContent T.Vertical $ vBox $ map renderLine $ _content st
      --
      ui =
        joinBorders $
        withBorderStyle unicode $
        B.borderWithLabel (str "GeminiBrowser") $
        dynamicLeftRightPad 3 $
        vBox [ hCenter $ padTopBottom 1 $ hLimit 30 searchField <=> str "Hit Enter to search"
            , contentArea
            , hCenter $ str "Not yet implemeted: Esc/Ctrl-q - quit, Ctrl-h - help"
            ]

renderLine :: Line -> Widget Name
renderLine (TextLine "")            = strWrap " "
renderLine (TextLine t)             = strWrap $ unpack t
renderLine (LinkLine t Nothing)     =
  withAttr linkAttr $ hyperlink (Txt.pack $ unpack t) . strWrap $ unpack t
renderLine (LinkLine t (Just s))    =
  withAttr linkAttr (hyperlink (Txt.pack $ unpack t) $ strWrap $ unpack s)
renderLine (TogglePreformatMode t)  = withAttr preformatAttr (strWrap $ unpack t)
renderLine (PreformattedTextLine t) = 
  withAttr preformatAttr $ strWrapWith preSetting (unpack t)
  -- Cuts off words longer than line. Mby horizontal viewport scrolling as well?
  where preSetting = defaultWrapSettings {preserveIndentation = True , breakLongWords = False}
renderLine (HeadingLine i t)        =
  vBox [strWrap $ concat (replicate (i-1) "  ") <> unpack t, B.hBorder ]
renderLine (UnorderedListLine t)    =
  strWrapWith listSetting $ unpack t
  where listSetting = defaultWrapSettings {fillStrategy = FillPrefix "  * ", fillScope = FillAll}
renderLine (QuoteLine t)            =
  withAttr quoteAttr $ strWrapWith qSetting $ unpack t
  where qSetting = defaultWrapSettings {fillStrategy = FillPrefix "  | ", fillScope = FillAll}


dynamicLeftRightPad :: Int -> Widget n -> Widget n
dynamicLeftRightPad = padLeftRight

handleEvent :: T.BrickEvent Name e -> EventM Name St ()
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl])) = return ()
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = focusRing %= F.focusNext
handleEvent (T.VtyEvent (V.EvKey V.KBackTab [])) = focusRing %= F.focusPrev
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
     []

app :: M.App St e Name
app = M.App
        { M.appDraw = drawUi
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const attrbMap
        }

attrbMap :: AttrMap
attrbMap =
  attrMap defAttr
    [ (linkAttr     , fg cyan)
    , (preformatAttr, fg brightBlack)
    , (quoteAttr    , fg magenta)]

linkAttr = attrName "link"
preformatAttr = attrName "preformatted"
quoteAttr = attrName "quote"


tuiRun :: IO St
tuiRun = do
  M.defaultMain app initialState

