{-# LANGUAGE OverloadedStrings #-}
module Protocol.Data.Request where

import Data.ByteString.Char8
import Network.URI hiding (scheme, authority, port, path, query, fragment)
import Data.Maybe (fromJust, fromMaybe)

data Url = Url { scheme :: ByteString
               , authority :: ByteString
               , port :: Int
               , path :: ByteString
               , query :: ByteString
               , fragment :: ByteString
               }
    | Relative { path :: ByteString
               , query :: ByteString
               , fragment :: ByteString
               } deriving (Eq, Show)

showUrl :: Url -> String
showUrl (Url scheme authority port path query frag)
  | scheme == "" && authority == "" = unpack (path <> query <> frag)
  | scheme == ""                    = unpack (authority <> path <> query <> frag)
  | otherwise                       = unpack (scheme <> "://" <> authority <> path <> query <> frag)
showUrl (Relative path query frag) = unpack (path <> query <> frag)

uriToUrl :: URI -> Url
uriToUrl uri
 | uriIsAbsolute uri = Url { scheme = pack scheme
                           , authority = pack $ uriRegName uriAuth
                           , port = port
                           , path = pack $ uriPath uri
                           , query = pack $ uriQuery uri
                           , fragment = pack $ uriFragment uri
                           }
  | otherwise   = Relative { path = pack $ uriPath uri
                           , query = pack $ uriQuery uri
                           , fragment = pack $ uriFragment uri
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
  | Relative {} <- url = nullURI { uriPath = unpack $ path url
                                 , uriQuery = unpack $ query url
                                 , uriFragment = unpack $ fragment url
                                 }
  | otherwise          = URI     { uriScheme = unpack scheme'
                                 , uriAuthority = Just nullURIAuth 
                                              { uriRegName = unpack $ authority url
                                              , uriPort = ':' : show (port url) }
                                 , uriPath = unpack $ path url
                                 , uriQuery = unpack $ query url
                                 , uriFragment = unpack $ fragment url
                                 }
  where 
    port' = if port url == 1965 then "" else ":" <> show (port url)
    scheme' = if scheme url == "" then "" else scheme url <> ":"

