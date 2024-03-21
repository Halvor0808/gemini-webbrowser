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
  show (InputExpected i)     = show i ++ " InputExpected"
  show (Success i)           = show i ++ " Success"
  show (Redirection i)       = show i ++ " Redirection"
  show (TemporaryFailure i)  = show i ++ " TemporaryFailure"
  show (PermanentFailure i)  = show i ++ " PermanentFailure"
  show (ClientCertificate i) = show i ++ " ClientCertificate"


getStatusCode :: Int -> Int -> StatusCode
getStatusCode x _
  | x == 1 = InputExpected x
  | x == 2 = Success x
  | x == 3 = Redirection x
  | x == 4 = TemporaryFailure x
  | x == 5 = PermanentFailure x
  | x == 6 = ClientCertificate x
  | otherwise = error "Invalid status code" -- shouldn't ever fail here. Should fail at `pStatusCode`
