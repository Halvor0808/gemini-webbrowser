{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Socket ( sockTests, getResponse ) where


import Network.Simple.TCP.TLS
import Network.TLS
import Protocol.Data.Request (Url(..), authority, path, showUrl)
import Protocol.Data.Response (Line(..), Response (..))
import Protocol.Parser.Response (pResponse)
import qualified Data.Attoparsec.ByteString.Lazy as AL (parseOnly)
import qualified Data.ByteString.Lazy.UTF8 as BLU (toString)
import qualified Data.ByteString.UTF8 as BSU (ByteString, toString, fromString)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)


addCallback :: ClientParams -> ClientParams
addCallback params = params { clientHooks = modi $ clientHooks params }
  where modi :: ClientHooks -> ClientHooks
        modi ch = ch { onServerCertificate = valCer }
        valCer _ _ _ _ = return []

retrievePage :: Url -> IO BSU.ByteString
retrievePage url = do
  let host = BSU.toString $ authority url
  params <- addCallback <$> newDefaultClientParams (host, ":1965")
  connect params host "1965" $ \(ctx,_) -> do
    send ctx ("gemini://" <> BSU.fromString host <> path url <> "\r\n")
    recvAll ctx
  where
    recvAll ctx = do
      recv ctx >>= \case
        Nothing -> return mempty
        Just chunk -> (chunk <>) <$> recvAll ctx



getResponse :: Url -> IO [Line]
getResponse url = getResponse' maxRedirects url url
  where maxRedirects = 6

getResponse' :: Int -> Url -> Url -> IO [Line]
getResponse' 0 target _ = return [TextLine $ "Error: Max redirects reached, when trying to reach" <> (BSU.fromString . showUrl) target]
getResponse' i target url = retrievePage url >>= handleResponse . BL.fromStrict
  where
    handleResponse response = 
      case AL.parseOnly pResponse response of
        Left err -> do
          return [TextLine $ BSU.fromString err <> " :\n", TextLine (BL.toStrict response)]
        Right response ->
          case response of
            INPUT _ _             -> return [TextLine "Input response"]
            SUCCESS _ _ lines     -> return lines
            REDIRECT _ newUrl     -> getResponse' (i-1) target newUrl
            ANY_FAIL code failMsg -> return [
              TextLine $ "Failed response: " <> BSU.fromString (show code) <>" :"<> failMsg]


-- TESTING
t0 = Url {scheme = "gemini", authority = "geminiprotocol.net", port = 1965, path = "/", query = "", fragment = ""}
t1 = Url {scheme = "gemini", authority = "kennedy.gemi.dev", port = 1965, path = "/kennedy.gmi", query = "", fragment = ""}
t2 = Url {scheme = "gemini", authority = "kennedy.gemi.dev", port = 1965, path = "/docs/search.gmi", query = "", fragment = ""}


sockTests = do
  -- print "Retrieving geminiprotocol.net"  -- does not work on Windows due to handling of TLS in socket.
  -- retrievePage t0 >>= print
  -- putStrLn "\n"
  print "Retrieving kennedy.gmi"
  retrievePage t1 >>= print
  putStrLn "\n"
  print "Retrieving search.gmi"
  retrievePage t2 >>= print
