{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T
import System.Environment (getArgs)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP (runTCPClient)


main :: IO ()
main = do
    [fname] <- getArgs
    printFile fname


printFile :: String -> IO ()
printFile fname = do 
    f <- readFile fname
    print $ lines f




--createSocket host port client = 
  --  runTCPClient withSocketDo $



tryClient :: IO ()
tryClient = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "Hello, world!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg
