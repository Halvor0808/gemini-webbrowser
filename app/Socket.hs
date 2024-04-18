{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Socket ( retrievePage ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C

import Network.Simple.TCP.TLS
import Network.TLS
import Control.Monad.IO.Class (liftIO)
import Protocol.Data.Request (Url, authority, path)


addCallback :: ClientParams -> ClientParams
addCallback params = params { clientHooks = modi $ clientHooks params }
  where modi :: ClientHooks -> ClientHooks
        modi ch = ch { onServerCertificate = valCer }
        valCer _ _ _ _ = return []

retrievePage :: Url -> IO C.ByteString
retrievePage url = do
  let host = C.unpack $ authority url
      urlPath = path url
  params <- addCallback <$> newDefaultClientParams (host, ":1965")
  connect params host "1965" $ \(ctx,_) -> do
    send ctx ("gemini://" <> C.pack host <> urlPath <> "\r\n")
    --
    -- recv ctx >>= \case
    --   Nothing -> return mempty
    --   Just chunk -> return chunk
    recvAll ctx
    where recvAll ctx = do
            recv ctx >>= \case
              Nothing -> return mempty
              Just chunk -> (chunk <>) <$> recvAll ctx
