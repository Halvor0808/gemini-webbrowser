{-# LANGUAGE OverloadedStrings #-}

module Socket (runConnection) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Simple.TCP.TLS
import Data.ByteString.Char8 (pack, unpack)

runConnection :: IO ()
runConnection = do
  params <- newDefaultClientParams ("geminiprotocol.net", ":1965")
  putStrLn $ show params
  connect params "geminiprotocol.net" "1965" $ \(context, sockAddr) -> do
    putStrLn "Connection established"
    send context "gemini://geminiprotocol.net/\r\n"
    msg <- recv context
    case msg of
      Nothing  -> putStrLn "Connection closed"
      (Just m) -> putStrLn (unpack m) 
  
{- TODO
 - Only support TLS 1.2 & 1.3 
 - clientSupported = Supported {supportedVersions = [TLS1.3,TLS1.2]}
 -

 -}