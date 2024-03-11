{-# LANGUAGE OverloadedStrings #-}

module Main where
--import qualified Data.Text as T
--import System.Environment (getArgs)
--import qualified Control.Exception as E
import qualified Data.Attoparsec.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8 
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)   
import Control.Applicative (many, (<|>), optional)

--TODO: 
{- Use `compareResults`to make functional tests
 - Do I have to deal with U+FEFF ("Byte order mark")?
 - Better fail messages -- using `fail`?
 - Optional parse-exection based on status codes?
 -}    


main :: IO ()
main = do
  tests

tests :: IO ()
tests = do
  testStatusCode 
  testCrlf
  testMime 
  testHeader

 
----------- Packet ---------------

data Packet = Packet { header :: Header
                     , body   :: String -- Temporary type
                     } deriving (Show, Eq)

pPacket :: Parse Packet
pPacket = do
  head <- pHeader
  -- add optionally how to execute based on what status code?


----------- HEADER ----------------

data Header = Header { status :: StatusCode
                     , mime   :: Maybe MIME
                     } deriving (Eq)

instance Show Header where
  show (Header s (Just m)) = show s ++ show m
  show (Header s Nothing ) = show s

pHeader :: Parser Header
pHeader = do
  status' <- pStatusCode
  _       <- char ' '
  mime'   <- optional pMime
  return $ Header status' mime' 


---------------MIME--------------------
data MIME = MIME 
  { mainType    :: B.ByteString
  , subType :: B.ByteString
  , parameters:: Maybe MIMEMeta
  } deriving (Eq)

instance Show MIME where
  show (MIME m s (Just p)) = unpack m ++ "/" ++ unpack s ++ showParameters p
  show (MIME m s Nothing ) = unpack m ++ "/" ++ unpack s

showParameters :: MIMEMeta -> String
showParameters []         = ""
showParameters xs = concatMap ((";"++) . show1Param) xs

show1Param :: (B.ByteString,B.ByteString)   -> String
show1Param (k,v) = unpack k ++ "=" ++ unpack v

makeMime :: Maybe B.ByteString -> Maybe B.ByteString -> Maybe MIMEMeta -> Maybe MIME 
makeMime Nothing _ _                = Nothing
makeMime _ Nothing _                = Nothing
makeMime (Just typ) (Just subtyp) p = Just $ MIME typ subtyp p

pMime :: Parser MIME
pMime = do
  typ     <- pMimeAlphabet
  _       <- char '/'
  subtype <- pMimeAlphabet 
  params  <- optional pParameters
  return $ MIME typ subtype params

mimeAlphabet :: String
mimeAlphabet = ['a'..'z' ] ++ ['A'..'Z'] 
            ++ ['0'..'9'] 
            ++  ".-+"

isMimeAlpha :: Char -> Bool
isMimeAlpha c = c `elem` mimeAlphabet

pMimeAlphabet :: Parser B.ByteString
pMimeAlphabet = BC.takeWhile1 isMimeAlpha

type MIMEMeta = [( B.ByteString, B.ByteString)]

pParameters :: Parser MIMEMeta
pParameters = do 
  params <- many1 pParam
  return $ params
    where
      pParam = do 
        key   <- char ';' *> pMimeAlphabet -- possibly a subset, only of chars?
        value <- char '=' *> pMimeAlphabet --possibly a subset, only chars
        return (key, value)

----------------- CRLF ----------------------------
crlf :: B.ByteString
crlf  = "\r\n"

-- End of line pattern
pCrlf :: Parser ()
pCrlf = do 
  _ <- "\r" *> "\n"
  return ()

------------------  Status Code   ----------------------------
--This feels unnecessarily tedious
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

pStatusCode :: Parser StatusCode
pStatusCode = do 
  num <- decimal <* pCrlf 
  case getStatusCode num of
      Just statusCode -> return statusCode
      Nothing         -> fail "pStatusCode" -- I would like to do something like this, but general for all parsers? 



----------------------  Other  -------------------------------------------------
endOfLine_fast :: (Eq a, Num a) => a -> Bool
endOfLine_fast w = w == 13 || w == 10


---------------------------TESTS--------------
testPipe              :: Parser a -> B.ByteString -> Either String a
testPipe parser input = eitherResult $ feed (parse parser input) B.empty

badParseTest  :: (Show a) => Parser a -> B.ByteString -> IO ()
badParseTest p input = putStrLn . show $ feed (parse p input) B.empty

-----StatusCode TEST
testStatusCode :: IO ()
testStatusCode = do 
  putStrLn $ "----- StatusCode: -----"
  badParseTest pStatusCode (pack "01\r\n")
  badParseTest pStatusCode (pack "20\r\n") 
  badParseTest pStatusCode (pack "35\r\n")
  badParseTest pStatusCode (pack "69\r\n")
  badParseTest pStatusCode (pack "70\r\n")

{-
  badParseTest pStatusCode header02
  badParseTest pStatusCode header03
-}


-----Mime TEST
testMime :: IO ()
testMime = do 
  putStrLn $ "----- Mime: -----"
  badParseTest pMime (pack "text/gemini") -- works
  badParseTest pMime (pack "audio/mpeg") -- works
  badParseTest pMime (pack "tex t/gemini") -- fails: note spacing
  badParseTest pMime (pack "fail/ª™§º©‘’&ŁŒıÐª") -- fails
  badParseTest pMime (pack "text/gemini;format=gemtext") -- works
  badParseTest pMime (pack "text/gemini;format=gemtext;name=myFile") -- works
  badParseTest pMime (pack "text/gemini; notRight=meta.typing") -- fails: note spacing

{-
--Mime Parameters TESTS
testParameters :: IO ()
testParameters = do
  putStrLn $ "----- Parameters: -----"
  badParseTest pParameters (pack ";format=markdown")  
  badParseTest pParameters (pack "; notRight=meta.typing") 
  badParseTest pParameters (pack ";typer=sub.type+mega;type2=subtype2;typ3=subtyp3; m istake=sub") 
  badParseTest pParameters (pack ";đu←↓→œ=wrong;")
-}

-----CRLF TEST
testCrlf :: IO ()
testCrlf = do
  putStrLn $ "----- Crlf: -----"
  badParseTest pCrlf crlf
  badParseTest pCrlf (pack $ "\n\rnope")
  badParseTest pCrlf (pack $ "\n\r ")


----- Header TESTS
testHeader :: IO ()
testHeader = do
  putStrLn "----- Header: -----"
  badParseTest pHeader (pack "20\r\n text/gemini;format=boring")
  badParseTest pHeader header01
  badParseTest pHeader header02
  badParseTest pHeader header03
  -- Add other test cases

--Header TESTS
header01, header02, header03 :: B.ByteString
header01 = "20\r\n"
header02 = "20 text/gemini\r\n"
header03 = "30 gemini://gemini.circumlunar.space\r\n"

{-
mime01 = feed (parse pMime (pack "text/gemini")) B.empty
mime02 = testPipe pMime (pack "text/gemini;format=gemtext;name=myFile")
mime03 = parseOnly pMime (pack "text/gemini;format=gemtext;name=myFile")
meta01 = (\(Right x) -> x) $ parseOnly pParameters (pack ";format=gemtext")
-}

