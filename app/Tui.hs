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

import qualified Data.ByteString.UTF8 as BSU (fromString, toString)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Txt
import Text.Wrap
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro
import qualified Data.Vector as Vec
import Protocol.Data.Response
import Protocol.Data.Request
import Socket (getResponse)
import Pages
import Network.URI
import Data.Maybe (fromJust, fromMaybe)
import Data.Default.Class  (Default(..))


data Name =  PageContent | ListContent| SearchField | HelpPage | HelpPageContent | History
  deriving (Ord, Eq, Show)

instance Default Name where
  def = SearchField

data St =
  St { _focusRing     :: F.FocusRing Name
     , _searchField   :: E.Editor String Name
     , _content       :: L.List Name Line
     , _currentPage   :: Url
     , _history       :: L.List Name Url
     , _previousFocus :: Name
     , _helpPageLines :: L.List Name Line
     } deriving (Show)
makeLenses ''St


------ Drawing functions ------

drawUi :: St -> [Widget Name]
drawUi st = do
  case F.focusGetCurrent (_focusRing st) of
    Just HelpPage -> [drawHelpPage st]
    Just History  -> [drawHistory st]
    Just _ -> [ui]
  where
    ui = joinBorders $ withBorderStyle unicode $
      B.borderWithLabel (str "GeminiBrowser") $
      vBox [ searchFieldArea
           , contentArea
           , B.hBorder
           , bottomText
           ]
    contentArea     = B.hBorder <=> padLeftRight 3 (padTop (Pad 1) (pageContent st))
    searchFieldArea = vLimitPercent 15 (hCenter (hLimit 65 searchField)
                      <=> hCenter (withDefAttr  helpAttr (str "Hit <Enter> to search")))
    searchField     = B.border $ F.withFocusRing (_focusRing st )
                      (E.renderEditor (str . unlines)) (_searchField st)
    bottomText      = hCenter $ withDefAttr helpAttr $ vBox
                      [ str "Esc/Ctrl-q = quit, Ctrl-e = toggle help. Ctrl-tab = cycle through focus areas."
                      , str ("Focus: " <> maybe "None" show (F.focusGetCurrent $ _focusRing st))
                      , str ("Current Line: " <> maybe "None" (show . snd) (L.listSelectedElement $ _content st))
                      ]
    drawHelpPage st = hCenter . B.borderWithLabel (str "Help Page") 
                      . viewport HelpPage T.Vertical . vLimit 35 $ 
                      L.renderList drawLine True (_helpPageLines st)
                      where 
                        drawLine True  line = forceAttr L.listSelectedFocusedAttr . visible $ renderLine line
                        drawLine False line = renderLine line 

pageContent :: St -> Widget Name
pageContent st =
  viewport PageContent T.Vertical . vLimit 30 $
  L.renderList drawListElement True (_content st)
  where
    isFocused = F.focusGetCurrent (_focusRing st) == Just PageContent
    drawListElement isSel line =
        case (isSel, isFocused) of
          (True, True)  -> forceAttr L.listSelectedFocusedAttr . visible $ renderLine line
          (True, False) -> forceAttr L.listSelectedAttr $ renderLine line
          _             -> renderLine line

drawHistory :: St -> Widget Name
drawHistory st =
  padTop (Pad 10) . hCenter . vLimitPercent 50 . hLimit 65  .
  B.borderWithLabel (str "History") $ list
  where list = L.renderListWithIndex renderUrl True (_history st)
        renderUrl i True url  = forceAttr L.listSelectedFocusedAttr . strWrap
                              $ show i <> ".  " <> showUrl url
        renderUrl i False url = withDefAttr linkAttr . strWrap
                              $ show i <> ".  " <> showUrl url

renderLine :: Line -> Widget n
renderLine (TextLine "") = strWrap " "
renderLine (TextLine t) = strWrap $ BSU.toString t
renderLine (LinkLine url Nothing) =
  withDefAttr linkAttr $ hyperlink (Txt.pack (show url)) . strWrap $ showUrl url
renderLine (LinkLine url (Just s)) =
  withDefAttr linkAttr (hyperlink (Txt.pack (show url)) $ strWrap $ BSU.toString s)
renderLine (TogglePreformatMode "") = withDefAttr preformatAttr (strWrap "```")
renderLine (TogglePreformatMode t) =
  withDefAttr preformatAttr (strWrap $ "``` " <> BSU.toString t)
renderLine (PreformattedTextLine t) =
  withDefAttr preformatAttr $ strWrapWith dontWrapSettings (BSU.toString t)
  where
    dontWrapSettings = defaultWrapSettings { preserveIndentation = True, breakLongWords = False }
renderLine (HeadingLine i t) =
  str (concat (replicate (i - 1) "@ "))
  <+> withDefAttr headingAttr (strWrap $ BSU.toString t)
renderLine (UnorderedListLine t) =
  str "  * " <+> strWrapWith wrapSettings (BSU.toString t)
renderLine (QuoteLine t) =
  withDefAttr quoteAttr  (str "  | ")
  <+> withDefAttr quoteAttr (strWrapWith wrapSettings $ BSU.toString t)

wrapSettings :: WrapSettings
wrapSettings = WrapSettings {fillStrategy = FillIndent 1, fillScope = FillAfterFirst, breakLongWords = True, preserveIndentation = False}

----- Event handling functions ------

handleEvent :: T.BrickEvent Name e -> EventM Name St ()
handleEvent (T.VtyEvent (V.EvKey  V.KEsc []))              = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) = 
  focusRing %= F.focusSetCurrent SearchField
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = togglePage HelpPage
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = togglePage History
handleEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just SearchField -> focusRing %= F.focusSetCurrent PageContent
    Just PageContent -> focusRing %= F.focusSetCurrent SearchField
    Just _ -> return ()
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just SearchField -> handleSearchFieldEvent ev
    Just PageContent -> handlePageContentEvent ev
    Just History     -> handleHistoryEvent ev
    Just HelpPage    -> handleHelpPageEvent ev
    _                 -> return ()

handleSearchFieldEvent :: T.BrickEvent Name e -> EventM Name St ()
handleSearchFieldEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
    sf <- use searchField
    let query = concat $ E.getEditContents sf
        url = case query of
          "home" ->  Right home
          _ -> case parseAbsoluteURI query of
              Just uri -> Right (uriToUrl uri)
              Nothing  -> Left ("Url is invalid: " <> query)
    handleQuery url
handleSearchFieldEvent ev = zoom searchField $  E.handleEditorEvent ev

handleQuery :: Either String Url -> EventM Name St ()
handleQuery (Left err) = do
  content .= mkList ListContent [TextLine (BSU.fromString err)]
handleQuery (Right url) = queryUrl url


handlePageContentEvent :: T.BrickEvent Name e -> EventM Name St ()
handlePageContentEvent ev =
  case ev of
    (T.VtyEvent ev@(V.EvKey V.KUp [])) -> do
        zoom content $ M.vScrollBy (M.viewportScroll PageContent) (-1)
        zoom content $ L.handleListEvent ev
    (T.VtyEvent ev@(V.EvKey V.KDown [])) -> do
        zoom content $ M.vScrollBy (M.viewportScroll PageContent) 1
        zoom content $ L.handleListEvent ev
    (T.VtyEvent ev@(V.EvKey V.KEnter [])) -> do
      list        <- use content
      presentPage <-  urlToUri <$> T.gets _currentPage
      let selectedLine = maybe (TextLine mempty) snd (L.listSelectedElement list)
      case selectedLine of
        LinkLine selectUrl _ ->
          let resolvedUri   = urlToUri selectUrl `Network.URI.relativeTo` presentPage
              urlAction url = do
                  queryUrl url
          in  (urlAction . uriToUrl) resolvedUri
        _ -> return ()
    _ -> return ()

handleHistoryEvent :: T.BrickEvent Name e -> EventM Name St ()
handleHistoryEvent ev =
  case ev of
    (T.VtyEvent (V.EvKey V.KEnter [])) -> do
      list <- use history
      let selectedUrl = maybe (error "should not happen") snd (L.listSelectedElement list)
      queryUrl selectedUrl
      togglePage History
    (T.VtyEvent e) -> zoom history $ L.handleListEvent e
    _ -> return ()

handleHelpPageEvent :: T.BrickEvent Name e -> EventM Name St ()
handleHelpPageEvent ev =
  case ev of
    (T.VtyEvent ev@(V.EvKey V.KUp [])) -> do
        zoom helpPageLines $ M.vScrollBy (M.viewportScroll HelpPage) (-1)
        zoom helpPageLines $ L.handleListEvent ev
    (T.VtyEvent ev@(V.EvKey V.KDown [])) -> do
        zoom helpPageLines $ M.vScrollBy (M.viewportScroll HelpPage) 1
        zoom helpPageLines $ L.handleListEvent ev
    _ -> return ()

togglePage :: Name -> EventM Name St ()
togglePage page = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just focus | page == focus
               -> use previousFocus >>= changeFocus
    Just _     -> changeFocus page

allTogglePages = [HelpPage, History]

changeFocus :: Name -> EventM Name St ()
changeFocus new = do
  r <- use focusRing
  previous <- use previousFocus
  let current = fromMaybe def (F.focusGetCurrent r)
  if current `elem` allTogglePages
    then focusRing %= F.focusSetCurrent new
    else do
      previousFocus .= current
      focusRing %= F.focusSetCurrent new

mkList :: n -> [a] -> L.List n a
mkList n ls = L.list n (Vec.fromList ls) 1

queryUrl :: Url -> EventM Name St ()
queryUrl url = do
  pushToHistory url
  searchField .= E.editor SearchField (Just 1) (showUrl url)
  case url of
    url | url == home -> startEvent
    _          -> do
      response <- liftIO $ getResponse url
      T.modify (currentPage .~ url)
      T.modify (content .~ mkList ListContent response)
  where
    pushToHistory :: Url -> EventM n St ()
    pushToHistory url = T.modify (history %~ L.listInsert 0 url)


----- Setup functions ------

initialState :: St
initialState =
  St (F.focusRing [SearchField, PageContent, HelpPage, History])
     (E.editor SearchField (Just 1) "home")
     (L.list PageContent (Vec.fromList []) 1)
     home
     (mkList History [home])
     PageContent
     helpPage

app :: M.App St e Name
app = M.App
        { M.appDraw = drawUi
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = startEvent
        , M.appAttrMap = const attrbMap
        }

helpPage :: L.List Name Line
helpPage = mkList HelpPageContent getHelpPage

startEvent :: EventM Name St ()
startEvent = do
    -- result <- liftIO getTestPage
    result <- liftIO getHomePage
    T.modify (content .~ mkList ListContent result)
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
