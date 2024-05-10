
module Main (main, unitTests) where

import Test.QuickCheck

import Protocol.Data.Request
import Network.URI
import Control.Monad (liftM3)
import Data.Char (isAlphaNum)
import qualified Data.ByteString.UTF8 as BSU
import Protocol.Parser.TestResponse
import Protocol.Parser.TestRequest
import Protocol.Parser.TestGemtextParser
import GHC.Base (IO)

main :: IO ()
main = do
  quickCheck prop_convertUrlToUri
  quickCheck prop_convertingUrlToUriAndBack

unitTests :: IO ()
unitTests = do
  testResponse
  testRequest
  testGemtextParser

instance Arbitrary Url where
    arbitrary = oneof [arbitraryUrl, arbitraryRelative]
      where
        arbitraryUrl =
          Url <$> arbitraryScheme
          <*> arbitraryAuthority
          <*> arbitraryPort
          <*> arbitraryPath
          <*> arbitraryQuery
          <*> arbitraryFragment

        arbitraryChar = elements ['\0'..'\127']
        arbitraryQuery    = BSU.fromString <$> arbitrary
        arbitraryFragment = BSU.fromString <$> arbitrary
        arbitraryRelative = Relative <$> arbitraryPath <*> arbitraryQuery <*> arbitraryFragment
        arbitraryScheme   = BSU.fromString . filterScheme <$> arbitrarySchemeChars
          where
            arbitrarySchemeChars = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-.")
            filterScheme = filter (\c -> isAlphaNum c || c `elem` "+-.")
        arbitraryAuthority = BSU.fromString . filterAuthority <$> arbitraryAuthorityChars
            where
                arbitraryAuthorityChars = listOf $ arbitraryChar `suchThat` notAllowedInAuthority
                notAllowedInAuthority c = c `notElem` "/?#\r\n"
                filterAuthority         = filter (\c -> isAlphaNum c || c `elem` "-_.~!$&()+*")
        arbitraryPort = choose (0, 65535)
        arbitraryPath = BSU.fromString . filterPath <$> arbitraryPathChars
          where
            arbitraryPathChars = listOf $ arbitraryChar `suchThat` notAllowedInPath
            notAllowedInPath c = not (isAlphaNum c || c `elem` "-_.~!$&'()*+,;=")
            filterPath = filter (\c -> isAlphaNum c || c `elem` "-_.~!$&()+*")


prop_convertUrlToUri :: Url -> Bool
prop_convertUrlToUri url = urlToUri (uriToUrl (urlToUri url)) == urlToUri url

prop_convertingUrlToUriAndBack :: Url -> Bool
prop_convertingUrlToUriAndBack url = uriToUrl (urlToUri url) == url