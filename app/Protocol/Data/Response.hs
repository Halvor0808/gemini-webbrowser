{-# LANGUAGE OverloadedStrings #-}

module Protocol.Data.Response where

import Data.ByteString
import qualified Data.ByteString.Char8 as B


data Packet = Packet { header :: Header
                     , body   :: ByteString -- should be [ByteString] ??
                     } deriving (Show, Eq)


data Header = Header { status :: StatusCode
                     , mime   :: Maybe MIME
                     } deriving (Eq)

instance Show Header where
  show (Header s (Just m)) = show s ++ " "++ show m
  show (Header s Nothing ) = show s


data MIME = MIME { mainType    :: ByteString
                 , subType :: ByteString
                 , parameters:: Maybe MIMEMeta
                 } deriving (Eq)
type MainMimeType = ByteString
type SubMimeType = ByteString

instance Show MIME where
  show (MIME m s (Just p)) = B.unpack m ++ "/" ++ B.unpack s ++ showParameters p
  show (MIME m s Nothing ) = B.unpack m ++ "/" ++ B.unpack s

type MIMEMeta = [( ByteString, ByteString)]

showParameters :: MIMEMeta -> String
showParameters []         = ""
showParameters xs = Prelude.concatMap ((";"++) . show1Param) xs
  where 
    show1Param :: (ByteString, ByteString) -> String
    show1Param (k,v) = B.unpack k ++ "=" ++ B.unpack v


data StatusCode = InputExpected     Int
                | Success           Int
                | Redirection       Int
                | TemporaryFailure  Int
                | PermanentFailure  Int
                | ClientCertificate Int
                deriving(Eq)

instance Show StatusCode where
  show (InputExpected i)     = show i
  show (Success i)           = show i
  show (Redirection i)       = show i
  show (TemporaryFailure i)  = show i
  show (PermanentFailure i)  = show i
  show (ClientCertificate i) = show i


getStatusCode :: Int -> Maybe StatusCode
getStatusCode x
  | x >= 10 && x < 20 = Just $ InputExpected x
  | x >= 20 && x < 30 = Just $ Success x
  | x >= 30 && x < 40 = Just $ Redirection x
  | x >= 40 && x < 50 = Just $ TemporaryFailure x
  | x >= 50 && x < 60 = Just $ PermanentFailure x
  | x >= 60 && x < 70 = Just $ ClientCertificate x
  | otherwise         = Nothing
