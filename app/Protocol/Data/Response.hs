{-# LANGUAGE OverloadedStrings #-}

module Protocol.Data.Response (
  Response(..),
  MIME, makeMime,
  Parameters(..),
  StatusCode(..), getStatusCode,
  Line(..),
) where

import Data.List (intercalate)
import qualified Data.ByteString.UTF8 as BSU
import Data.Default.Class

import Protocol.Data.Request (Url)

data Response = INPUT       {_statusCode :: StatusCode, _prompt  :: BSU.ByteString}
              | SUCCESS     {_statusCode :: StatusCode, _mime    :: Maybe MIME,  _lines :: [Line]}
              | REDIRECT    {_statusCode :: StatusCode, _url     :: Url       }
              | ANY_FAIL    {_statusCode :: StatusCode, _failMsg :: BSU.ByteString} -- temporary replacement for error codes 40,50,60
              -- | TEMP_FAIL   StatusCode     FailureMessage
              -- | PERM_FAIL   StatusCode     FailureMessage
              -- | CLIENT_CERT StatusCode     FailureMessage
              deriving (Eq, Show)

data Line = TextLine             { _text        :: BSU.ByteString }
          | TogglePreformatMode  { _ByteString  :: BSU.ByteString }
          | PreformattedTextLine { _text        :: BSU.ByteString }
          | UnorderedListLine    { _text        :: BSU.ByteString }
          | QuoteLine            { _text        :: BSU.ByteString }
          | HeadingLine          { _level       :: Int        , _text        ::       BSU.ByteString}
          | LinkLine             { _link        :: Url        , _displayText :: Maybe BSU.ByteString}
          deriving (Show, Eq)

data StatusCode = InputCode              Int Int
                | SuccessCode            Int Int
                | RedirCode              Int Int
                | TempFailCode           Int Int
                | PermanentFailCode      Int Int
                | RequireCertificateCode Int Int
                deriving(Eq)
                
instance Show StatusCode where
  show (InputCode x y)              = show x ++ show y
  show (SuccessCode x y)            = show x ++ show y
  show (RedirCode x y)              = show x ++ show y
  show (TempFailCode x y)           = show x ++ show y
  show (PermanentFailCode x y)      = show x ++ show y
  show (RequireCertificateCode x y) = show x ++ show y

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

type MainMimeType = BSU.ByteString
type SubMimeType = BSU.ByteString
data MIME = MIME { _mainType   ::  MainMimeType
                 , _subType    ::  SubMimeType
                 , _parameters ::  Maybe Parameters
                 } deriving (Eq)

instance Default MIME where
  def = MIME "text" "gemini" (Just $ Parameters [("charset", "utf-8")])

makeMime :: Maybe (MainMimeType, SubMimeType) -> Maybe Parameters -> Maybe MIME
makeMime Nothing        Nothing = Just def
makeMime (Just (typ, subtyp)) p = Just $ MIME typ subtyp p
makeMime Nothing              _ = Nothing

instance Show MIME where
  show (MIME m s (Just p)) = BSU.toString m ++ "/" ++ BSU.toString s ++ show p
  show (MIME m s Nothing ) = BSU.toString m ++ "/" ++ BSU.toString s

newtype Parameters = Parameters [(BSU.ByteString, BSU.ByteString)]
  deriving (Eq)

instance Show Parameters where
  show (Parameters []) = ""
  show (Parameters xs) = ("[" ++) . ( ++ "]") . intercalate ", " $ map showParam xs
    where
      showParam :: (BSU.ByteString, BSU.ByteString) -> String
      showParam (k,v) = BSU.toString k ++ "=" ++ BSU.toString v
