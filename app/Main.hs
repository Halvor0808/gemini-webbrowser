{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.Protocol.Parser.TestResponse (tests)
import Test.Protocol.Parser.TestRequest

main :: IO ()
main = do 
  putStrLn "--- Main has been run! ---\n"
  tests -- Response tests
  putStrLn "----- Request: -----"
  testRequest

