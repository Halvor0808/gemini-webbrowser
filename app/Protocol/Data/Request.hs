{-# LANGUAGE OverloadedStrings #-}
module Protocol.Data.Request where

import qualified Data.ByteString.UTF8 as BSU
import Network.URI
    ( nullURI,
      nullURIAuth,
      uriIsAbsolute,
      URI(..),
      URIAuth(uriPort, uriRegName) )
import Data.Maybe (fromJust, fromMaybe)

data Url = Url { scheme    :: BSU.ByteString
               , authority :: BSU.ByteString
               , port      :: Int
               , path      :: BSU.ByteString
               , query     :: BSU.ByteString
               , fragment  :: BSU.ByteString
               }
    | Relative { path      :: BSU.ByteString
               , query     :: BSU.ByteString
               , fragment  :: BSU.ByteString
               } deriving (Eq, Show)

showUrl :: Url -> String
showUrl (Url scheme authority port path query frag)
  | scheme == "" && authority == "" = BSU.toString (path <> query <> frag)
  | scheme == ""                    = BSU.toString (authority <> path <> query <> frag)
  | otherwise                       = BSU.toString (scheme <> "://" <> authority <> path <> query <> frag)
showUrl (Relative path query frag)  = BSU.toString (path <> query <> frag)

uriToUrl :: URI -> Url
uriToUrl uri
 | uriIsAbsolute uri = Url { scheme     =  BSU.fromString scheme
                           , authority  =  BSU.fromString $ uriRegName uriAuth
                           , port       = port
                           , path       =  BSU.fromString $ uriPath uri
                           , query      =  BSU.fromString $ uriQuery uri
                           , fragment   =  BSU.fromString $ uriFragment uri
                           }
  | otherwise   = Relative { path       =  BSU.fromString $ uriPath uri
                           , query      =  BSU.fromString $ uriQuery uri
                           , fragment   =  BSU.fromString $ uriFragment uri
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
  | Relative {} <- url = nullURI { uriPath      = BSU.toString $ path url
                                 , uriQuery     = BSU.toString $ query url
                                 , uriFragment  = BSU.toString $ fragment url
                                 }
  | otherwise          = URI     { uriScheme    = BSU.toString scheme'
                                 , uriAuthority = Just nullURIAuth 
                                              { uriRegName = BSU.toString $ authority url
                                              , uriPort = ':' : show (port url) }
                                 , uriPath      = BSU.toString $ path url
                                 , uriQuery     = BSU.toString $ query url
                                 , uriFragment  = BSU.toString $ fragment url
                                 }
  where 
    port' = if port url == 1965 then "" else ":" <> show (port url)
    scheme' = if scheme url == "" then "" else scheme url <> ":"

