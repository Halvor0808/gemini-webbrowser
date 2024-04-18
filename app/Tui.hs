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

import Protocol.Data.Gemtext (Line(..))
import Protocol.Parser.Response (pResponse)
import qualified Data.ByteString.Char8 as C8
import Protocol.Data.Response
import Socket (retrievePage)
import Protocol.Data.Request (Url(..))
import Protocol.Parser.Request
import Homepage (getHomePage)
import GHC.TypeError (ErrorMessage(Text))


data Name = PageContent | SearchField
  deriving (Ord, Eq, Show)

data St =
  St { _focusRing   :: F.FocusRing Name
     , _searchField :: E.Editor String Name
     , _content     :: [Line]
     } deriving (Show)
makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [ui]
  where
    searchField = B.border $
      F.withFocusRing (_focusRing st) (E.renderEditor (str . unlines)) (_searchField st)
    contentArea = vBox
      [ B.hBorderWithLabel (str "Page-content")
      , padLeftRight 3 $ padTop (Pad 1) $
        viewport PageContent T.Vertical (vBox . map renderLine $ _content st)
      ]
    ui = joinBorders $ withBorderStyle unicode $
      B.borderWithLabel (str "GeminiBrowser") $
      vBox
        [ vLimitPercent 15 (hCenter (hLimit 65 searchField)
        <=> hCenter (withAttr helpAttr (str "Hit Enter to search")))
        , contentArea
        , B.hBorder
        , hCenter $ withAttr helpAttr $
          str "Esc/Ctrl-q - quit, Ctrl-h - help (Not yet implemented)"
        ]

renderLine :: Line -> Widget Name
renderLine (TextLine "") = strWrap " "
renderLine (TextLine t) = strWrap $ unpack t
renderLine (LinkLine t Nothing) =
  withAttr linkAttr $ hyperlink (Txt.pack $ unpack t) . strWrap $ unpack t
renderLine (LinkLine t (Just s)) =
  withAttr linkAttr (hyperlink (Txt.pack $ unpack t) $ strWrap $ unpack s)
renderLine (TogglePreformatMode "") = str ""
renderLine (TogglePreformatMode t) =
  withAttr preformatAttr (strWrap $ unpack t)
renderLine (PreformattedTextLine t) =
  withAttr preformatAttr $ strWrapWith preSetting (unpack t)
  where
    preSetting = defaultWrapSettings { preserveIndentation = True, breakLongWords = False }
renderLine (HeadingLine i t) =
  vBox [str "\n", indent <+> withAttr headingAttr (strWrap $ unpack t)]
  where
    indent = str $ concat (replicate (i - 1) "@ ")
renderLine (UnorderedListLine t) =
  strWrapWith listSetting $ unpack t
  where
    listSetting = defaultWrapSettings { fillStrategy = FillPrefix "  * ", fillScope = FillAll }
renderLine (QuoteLine t) =
  withAttr quoteAttr $ strWrapWith qSetting $ unpack t
  where
    qSetting = defaultWrapSettings { fillStrategy = FillPrefix "  | ", fillScope = FillAll }

handleEvent :: T.BrickEvent Name e -> EventM Name St ()
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl])) = return ()
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent (T.VtyEvent (V.EvKey V.KUp [])) =
  zoom content $ M.vScrollBy (M.viewportScroll PageContent) (-1)
handleEvent (T.VtyEvent (V.EvKey V.KDown [])) =
  zoom content $ M.vScrollBy (M.viewportScroll PageContent) 1
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just PageContent -> handleEventPageContent ev
    Just SearchField ->  case ev of
              (T.VtyEvent (V.EvKey V.KEnter []))
                -> do
                  sf <- use searchField
                  let query = concat $ E.getEditContents sf
                  if query == "home" then startEvent 
                  else
                    case getUrl query of
                      Left e -> do 
                        T.modify (content .~ [TextLine (pack e)])
                      Right url -> do
                        -- Why won't this line execute?
                        T.modify (content .~ [TextLine $ "Fetching " <> pack (show url)  <> " ..."])
                        response <- liftIO $ getResponse url
                        T.modify (content .~ response)
              _ ->  zoom searchField $  E.handleEditorEvent ev
    Nothing -> return ()

handleEventPageContent :: T.BrickEvent Name e -> EventM Name St ()
handleEventPageContent _ = return ()

getUrl :: String -> Either String Url
getUrl query = do
  case parseOnly pUrl (pack query) of
    Left err -> Left $ "invalid url parse:" <> err
    Right url -> return url

getResponse :: Url -> IO [Line]
getResponse url = do
  response <- retrievePage url
  case parseOnly pResponse response of
    Left err -> do
      return [TextLine $ "invalid response?: " <> pack err <> " :\n" <> pack (show response)]
    Right response ->
      case response of
        INPUT _ _ -> return [TextLine "Input response"]
        SUCCESS _ _ lines -> return lines
        REDIRECT _ newUrl -> return [TextLine $ "Redirect to" <> pack (show newUrl)]
        ANY_FAIL code failMsg -> return [
          TextLine $ "Failed response: " <> pack (show code) <>" :"<> failMsg]

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
        , M.appStartEvent = startEvent
        , M.appAttrMap = const attrbMap
        }

startEvent :: EventM Name St ()
startEvent = do
    result <- liftIO getHomePage
    T.modify (content .~ result)
    return ()

attrbMap :: AttrMap
attrbMap =
  attrMap defAttr
    [ (linkAttr     , fg cyan)
    , (preformatAttr, fg brightBlack)
    , (quoteAttr    , fg magenta)
    , (helpAttr     , green `on` black)
    , (headingAttr  , withStyle (style underline) bold )
    ]

linkAttr = attrName "link"
preformatAttr = attrName "preformatted"
quoteAttr = attrName "quote"
helpAttr = attrName "help"
headingAttr = attrName "heading"


tuiRun :: IO St
tuiRun = do
  M.defaultMain app initialState
