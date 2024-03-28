{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Protocol.Parser.TestResponse (testResponse)
import Test.Protocol.Parser.TestRequest (testRequest)

main :: IO ()
main = do 
  putStrLn "--- Main has been run! ---\n"
  testResponse
  putStrLn "----- Request: -----"
  testRequest


