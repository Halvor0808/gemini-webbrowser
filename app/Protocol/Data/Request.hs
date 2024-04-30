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
          | RelUrl { path :: ByteString
                   , query :: ByteString
                   , fragment :: ByteString
                   } deriving (Eq, Show)

showUrl :: Url -> String
showUrl (Url "" "" _ path query frag) = unpack (path <> query <> frag)
showUrl (Url "" authority _ path query frag) = 
  unpack (authority <> path <> query <> frag)
showUrl (Url scheme authority _ path query frag) = 
  unpack (scheme <> "://" <> authority <> path <> query <> frag)

uriToUrl :: URI -> Url
uriToUrl uri =
    if uriIsAbsolute uri
        then
            let uriAuth = fromMaybe nullURIAuth (uriAuthority uri)
                port    = case uriPort uriAuth of
                            "" -> 1965
                            ':':xs -> read $ Prelude.tail xs
                            x -> read $ Prelude.tail x
                scheme = Prelude.init $ uriScheme uri
            in
            Url { scheme = pack scheme
                , authority = pack $ uriRegName uriAuth
                , port = port
                , path = pack $ uriPath uri
                , query = pack $ uriQuery uri
                , fragment = pack $ uriFragment uri
                }
        else
            RelUrl { path = pack $ uriPath uri
                   , query = pack $ uriQuery uri
                   , fragment = pack $ uriFragment uri
                   }
