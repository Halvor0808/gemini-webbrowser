{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Protocol.Parser.TestResponse (testResponse)
import Test.Protocol.Parser.TestRequest (testRequest)
import Tui.Tui (tuiRun)

main :: IO ()
main = do 
  tests 
  tuiRun
  

tests :: IO ()
tests = do
  putStrLn "--- Main has been run! ---\n"
  testResponse
  putStrLn "----- Request: -----"
  testRequest


