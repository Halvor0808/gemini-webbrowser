{-# LANGUAGE OverloadedStrings #-}

module Protocol.Data.Response (
  Packet(..), Header(..), 
  makeMime, MIME, MainMimeType, SubMimeType, MIMEMeta, 
  StatusCode(..), getStatusCode
) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B
import Protocol.Data.Gemtext (Line)


data Packet = Packet { header :: Header
                     , body   :: [Line]
                     } deriving (Show, Eq)


data Header = Header { status :: StatusCode
                     , mime   :: Maybe MIME
                     } deriving (Eq)

instance Show Header where
  show (Header s (Just m)) = show s ++ " "++ show m
  show (Header s Nothing ) = show s


type MainMimeType = ByteString
type SubMimeType = ByteString
data MIME = MIME { mainType   :: MainMimeType
                 , subType    :: SubMimeType
                 , parameters :: Maybe MIMEMeta
                 } deriving (Eq)

makeMime :: Maybe MainMimeType -> Maybe SubMimeType -> Maybe MIMEMeta -> Maybe MIME
makeMime Nothing Nothing p          = Just $ MIME "text" "gemini" p
makeMime Nothing _ _                = Nothing
makeMime _ Nothing _                = Nothing
makeMime (Just typ) (Just subtyp) p = Just $ MIME typ subtyp p

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


data StatusCode = InputExpected     Int Int 
                | Success           Int Int 
                | Redirection       Int Int 
                | TemporaryFailure  Int Int 
                | PermanentFailure  Int Int 
                | ClientCertificate Int Int 
                deriving(Eq, Show)

getStatusCode :: Int -> Int -> StatusCode
getStatusCode x y
  | x == 1 = InputExpected x y
  | x == 2 = Success x y
  | x == 3 = Redirection x y
  | x == 4 = TemporaryFailure x y
  | x == 5 = PermanentFailure x y
  | x == 6 = ClientCertificate x y
  | otherwise = error "Invalid status code" -- shouldn't ever fail here. Should fail at `pStatusCode`
