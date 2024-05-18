{-# LANGUAGE OverloadedStrings #-}
module Protocol.Data.Request where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Network.URI
    ( nullURI,
      nullURIAuth,
      uriIsAbsolute,
      URI(..),
      URIAuth(uriPort, uriRegName) )
import Data.Maybe (fromJust, fromMaybe)

data Url = Url { scheme    :: BLU.ByteString
               , authority :: BLU.ByteString
               , port      :: Int
               , path      :: BLU.ByteString
               , query     :: BLU.ByteString
               , fragment  :: BLU.ByteString
               }
    | Relative { path      :: BLU.ByteString
               , query     :: BLU.ByteString
               , fragment  :: BLU.ByteString
               } deriving (Eq, Show)

showUrl :: Url -> String
showUrl (Url scheme authority port path query frag)
  | scheme == "" && authority == "" = BLU.toString (path <> query <> frag)
  | scheme == ""                    = BLU.toString (authority <> path <> query <> frag)
  | otherwise                       = BLU.toString (scheme <> "://" <> authority <> path <> query <> frag)
showUrl (Relative path query frag)  = BLU.toString (path <> query <> frag)

uriToUrl :: URI -> Url
uriToUrl uri
 | uriIsAbsolute uri = Url { scheme     =  BLU.fromString scheme
                           , authority  =  BLU.fromString $ uriRegName uriAuth
                           , port       = port
                           , path       =  BLU.fromString $ uriPath uri
                           , query      =  BLU.fromString $ uriQuery uri
                           , fragment   =  BLU.fromString $ uriFragment uri
                           }
  | otherwise   = Relative { path       =  BLU.fromString $ uriPath uri
                           , query      =  BLU.fromString $ uriQuery uri
                           , fragment   =  BLU.fromString $ uriFragment uri
                           }
  where
    scheme = Prelude.init $ uriScheme uri
    uriAuth = fromMaybe nullURIAuth (uriAuthority uri)
    port    = case uriPort uriAuth of
                "" -> 1965
                ':':xs -> read xs
                x -> read x

urlToUri :: Url -> URI
urlToUri url
  | Relative {} <- url = nullURI { uriPath      = BLU.toString $ path url
                                 , uriQuery     = BLU.toString $ query url
                                 , uriFragment  = BLU.toString $ fragment url
                                 }
  | otherwise          = URI     { uriScheme    = BLU.toString scheme'
                                 , uriAuthority = Just nullURIAuth 
                                              { uriRegName = BLU.toString $ authority url
                                              , uriPort = ':' : show (port url) }
                                 , uriPath      = BLU.toString $ path url
                                 , uriQuery     = BLU.toString $ query url
                                 , uriFragment  = BLU.toString $ fragment url
                                 }
  where 
    port' = if port url == 1965 then "" else ":" <> show (port url)
    scheme' = if scheme url == "" then "" else scheme url <> ":"

