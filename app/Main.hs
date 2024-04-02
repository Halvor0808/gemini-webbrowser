{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Protocol.Parser.TestResponse (testResponse)
import Test.Protocol.Parser.TestRequest (testRequest)
import Tui.Tui (tuiRun)

main :: IO ()
main = do
  putStrLn "--- Main has been run! ---\n"
  tests 
  st <- tuiRun
  print st


tests :: IO ()
tests = do
  testResponse
  putStrLn "----- Request: -----"
  testRequest


