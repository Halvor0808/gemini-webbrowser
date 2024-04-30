{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui (tuiRun) where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F
import qualified Brick.Types as T
import qualified Brick.Main as M
import Brick.Widgets.Border.Style (unicode)
import Brick.AttrMap (attrMap, AttrMap, attrName, AttrName)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Center (hCenter, center)
import Brick.Widgets.Core
import Brick.Util ( on, fg, bg )
import Brick (style)

import Graphics.Vty.Attributes
import qualified Graphics.Vty as V

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Txt
import Text.Wrap
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro
import qualified Data.Vector as Vec
import Protocol.Parser.Response (pResponse)
import qualified Data.ByteString.Char8 as C8
import Protocol.Data.Response
import Socket (retrievePage)
import Protocol.Data.Request
import Protocol.Parser.Request
import Pages
import Network.URI (parseAbsoluteURI)
import Data.Maybe (fromJust)


data Name =  PageContent | ListContent| SearchField | HelpPage
  deriving (Ord, Eq, Show)

data St =
  St { _focusRing   :: F.FocusRing Name
     , _searchField :: E.Editor String Name
     , _content     :: L.List Name Line
     } deriving (Show)
makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = do
  case F.focusGetCurrent (_focusRing st) of
    Just HelpPage    -> helpPage
    Just _ -> [ui]
  where
    ui = joinBorders $ withBorderStyle unicode $
      B.borderWithLabel (str "GeminiBrowser") $
      vBox [ searchFieldArea
           , contentArea
           , B.hBorder
           , bottomText
           ]
    contentArea = B.hBorderWithLabel (str "Page-content")
                  <=> padLeftRight 3 (padTop (Pad 1) (pageContent st))
    searchFieldArea = vLimitPercent 15 (hCenter (hLimit 65 searchField)
                    <=> hCenter (withDefAttr  helpAttr (str "Hit <Enter> to search")))
    searchField = B.border $ F.withFocusRing (_focusRing st )
                  (E.renderEditor (str . unlines)) (_searchField st)
    bottomText = hCenter $ withDefAttr helpAttr $ vBox
             [ str "Esc/Ctrl-q = quit, Ctrl-e = toggle help. Ctrl-tab = cycle through focus areas."
             , str ("Focus: " <> maybe "None" show (F.focusGetCurrent $ _focusRing st))
             , str ("Current Line: " <> maybe "None" (show . snd) (L.listSelectedElement $ _content st))
             ]
    helpPage = [viewport HelpPage T.Vertical $ str getHelpPage]

pageContent :: St -> Widget Name
pageContent st =
  viewport PageContent T.Vertical $ vLimit 30 $ -- 30 is just a guessed number
  L.renderList drawListElement True (_content st)
  where
    isFocused = F.focusGetCurrent (_focusRing st) == Just PageContent
    drawListElement isSel line =
        case (isSel, isFocused) of
          (True,True)  -> forceAttr L.listSelectedFocusedAttr . visible $ renderLine line
          (True,False) -> forceAttr L.listSelectedAttr $ renderLine line
          _            -> renderLine line

renderLine :: Line -> Widget Name
renderLine (TextLine "") = strWrap " "
renderLine (TextLine t) = strWrap $ unpack t
renderLine (LinkLine url Nothing) =
  withDefAttr linkAttr $ hyperlink (Txt.pack (show url)) . strWrap $ showUrl url
renderLine (LinkLine url (Just s)) =
  withDefAttr linkAttr (hyperlink (Txt.pack (show url)) $ strWrap $ unpack s)
renderLine (TogglePreformatMode "") = withDefAttr preformatAttr (strWrap "```")
renderLine (TogglePreformatMode t) =
  withDefAttr preformatAttr (strWrap $ "``` " <> unpack t)
renderLine (PreformattedTextLine t) =
  withDefAttr preformatAttr $ strWrapWith preSetting (unpack t)
  where
    preSetting = defaultWrapSettings { preserveIndentation = True, breakLongWords = False }
renderLine (HeadingLine i t) =
  vBox [str "\n", indent <+> withDefAttr  headingAttr (strWrap $ unpack t)]
  where indent = str $ concat (replicate (i - 1) "@ ")
renderLine (UnorderedListLine t) =
  strWrapWith listSetting $ unpack t
  where
    listSetting = defaultWrapSettings { fillStrategy = FillPrefix "  * ", fillScope = FillAll }
renderLine (QuoteLine t) =
  withDefAttr  quoteAttr $ strWrapWith qSetting $ unpack t
  where
    qSetting = defaultWrapSettings { fillStrategy = FillPrefix "  | ", fillScope = FillAll }

handleEvent :: T.BrickEvent Name e -> EventM Name St ()
handleEvent (T.VtyEvent (V.EvKey  V.KEsc []))             = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just HelpPage -> focusRing    %= F.focusSetCurrent SearchField
    Just SearchField -> focusRing %= F.focusSetCurrent HelpPage
    Just _ -> return ()
handleEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just SearchField -> focusRing %= F.focusSetCurrent PageContent
    Just PageContent -> focusRing %= F.focusSetCurrent SearchField
    Just _ -> return ()
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just SearchField ->  handleSearchFieldEvent ev
    Just PageContent -> handlePageContentEvent ev
    _ -> return ()

handleSearchFieldEvent :: T.BrickEvent Name e -> EventM Name St ()
handleSearchFieldEvent ev = case ev of
      (T.VtyEvent (V.EvKey V.KEnter []))
        -> do
          sf <- use searchField
          let query = concat $ E.getEditContents sf
          if query == "home"
            then startEvent
            else queryUrl (uriToUrl . fromJust $ parseAbsoluteURI query)
      _ ->  zoom searchField $  E.handleEditorEvent ev

handlePageContentEvent :: T.BrickEvent Name e -> EventM Name St ()
handlePageContentEvent ev =
  case ev of
    (T.VtyEvent ev@(V.EvKey V.KUp []))
      -> do
        zoom content $ M.vScrollBy (M.viewportScroll PageContent) (-1)
        zoom content $ L.handleListEvent ev
    (T.VtyEvent ev@(V.EvKey V.KDown []))
      -> do
        zoom content $ M.vScrollBy (M.viewportScroll PageContent) 1
        zoom content $ L.handleListEvent ev
    (T.VtyEvent ev@(V.EvKey V.KEnter []))
      -> do
        s <- use content
        let line = maybe (TextLine mempty) snd (L.listSelectedElement s)
        case line of
          LinkLine url _ -> do
            queryUrl url
            searchField .= E.editor SearchField (Just 1) (showUrl url)
          _ -> return ()
    _ -> return ()


mkList :: [Line] -> L.List Name Line
mkList ls = L.list ListContent (Vec.fromList ls) 1

queryUrl :: Url -> EventM Name St ()
queryUrl url = do
  do response <- liftIO $ getResponse url
     T.modify (content .~ mkList response)

getResponse :: Url -> IO [Line]
getResponse url = do
  response <- retrievePage url
  case parseOnly pResponse response of
    Left err -> do
      return [TextLine $ "invalid response?: " <> pack err <> " :\n" <> pack (show response)]
    Right response ->
      case response of
        INPUT _ _             -> return [TextLine "Input response"]
        SUCCESS _ _ lines     -> return lines
        REDIRECT _ newUrl     -> return [TextLine $ "Redirect to" <> pack (show newUrl)]
        ANY_FAIL code failMsg -> return [
          TextLine $ "Failed response: " <> pack (show code) <>" :"<> failMsg]

initialState :: St
initialState =
  St (F.focusRing [SearchField, PageContent, HelpPage])
     (E.editor SearchField (Just 1) "")
     (L.list PageContent (Vec.fromList []) 1)

app :: M.App St e Name
app = M.App
        { M.appDraw = drawUi
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = startEvent
        , M.appAttrMap = const attrbMap
        }

startEvent :: EventM Name St ()
startEvent = do
    -- result <- liftIO getTestPage
    result <- liftIO getHomePage
    T.modify (content .~ mkList result)
    return ()

attrbMap :: AttrMap
attrbMap =
  attrMap defAttr
    [ (linkAttr         , fg cyan)
    , (preformatAttr    , fg brightBlack)
    , (quoteAttr        , fg magenta)
    , (helpAttr         , green `on` black)
    , (headingAttr      , withStyle (style underline) bold )
    , (E.editFocusedAttr, white `on` cyan)
    , (L.listSelectedAttr, white `on` brightBlack)
    , (L.listSelectedFocusedAttr, white `on` cyan)
    ]

linkAttr = attrName "link"
preformatAttr = attrName "preformatted"
quoteAttr = attrName "quote"
helpAttr = attrName "help"
headingAttr = attrName "heading"



tuiRun :: IO St
tuiRun = do
  M.defaultMain app initialState
