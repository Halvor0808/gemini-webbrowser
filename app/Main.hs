{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Protocol.Parser.TestResponse (testResponse)
import Test.Protocol.Parser.TestRequest (testRequest)
import Tui (tuiRun)
import Socket (
  retrievePage
  )
import Protocol.Data.Request (Url(..))

main :: IO ()
main = do
  putStrLn "--- Main has been run! ---\n"
  st <- tuiRun
  print st


tests :: IO ()
tests = do
  putStrLn " --- /// Running tests /// --- \n"
  testResponse
  testRequest


