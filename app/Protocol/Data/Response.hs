{-# LANGUAGE OverloadedStrings #-}

module Protocol.Data.Response (
  Response(..),
  MIME, makeMime,
  Parameters(..),
  StatusCode(..), getStatusCode,
  Prompt, 
  FailureMessage,
) where

import Data.ByteString ( ByteString)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as B

import Protocol.Data.Gemtext (Line)
import Protocol.Data.Request (Url)


type Prompt = ByteString
type FailureMessage = ByteString

data Response = INPUT       StatusCode     Prompt
              | SUCCESS     StatusCode     (Maybe MIME)      [Line]
              | REDIRECT    StatusCode     Url
              | ANY_FAIL    StatusCode     FailureMessage -- temporary replacement for error codes 40,50,60
              -- | TEMP_FAIL   StatusCode     FailureMessage
              -- | PERM_FAIL   StatusCode     FailureMessage
              -- | CLIENT_CERT StatusCode     FailureMessage
              deriving (Eq, Show)

data StatusCode = InputCode              Int Int
                | SuccessCode            Int Int
                | RedirCode              Int Int
                | TempFailCode           Int Int
                | PermanentFailCode      Int Int
                | RequireCertificateCode Int Int
                deriving(Eq, Show)

getStatusCode :: Int -> Int -> StatusCode
getStatusCode x y =
  case x of
    1 -> InputCode x y
    2 -> SuccessCode x y
    3 -> RedirCode x y
    4 -> TempFailCode x y
    5 -> PermanentFailCode x y
    6 -> RequireCertificateCode x y
    _ -> error $ "Invalid status code " <> show x <> show y -- shouldn't ever fail here. Should fail at `pStatusCode`

type MainMimeType = ByteString
type SubMimeType = ByteString
data MIME = MIME MainMimeType SubMimeType (Maybe Parameters)
    deriving (Eq)

makeMime :: Maybe (MainMimeType, SubMimeType) -> Maybe Parameters -> Maybe MIME
makeMime Nothing        Nothing = Just $ MIME "text" "gemini" (Just $ Parameters [("charset", "utf-8")]) -- default to text/gemini
makeMime (Just (typ, subtyp)) p = Just $ MIME typ subtyp p
makeMime Nothing              _ = Nothing

instance Show MIME where
  show (MIME m s (Just p)) = B.unpack m ++ "/" ++ B.unpack s ++ show p
  show (MIME m s Nothing ) = B.unpack m ++ "/" ++ B.unpack s

newtype Parameters = Parameters [(ByteString, ByteString)]
  deriving (Eq)

instance Show Parameters where
  show (Parameters []) = ""
  show (Parameters xs) = ("[" ++) . ( ++ "]") . intercalate ", " $ map showParam xs
    where
      showParam :: (ByteString, ByteString) -> String
      showParam (k,v) = B.unpack k ++ "=" ++ B.unpack v
