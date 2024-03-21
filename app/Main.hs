{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse.Protocol.Response
import Parse.Protocol.Request

main :: IO ()
main = do 
  putStrLn "--- Main has been run! ---\n"
  tests -- Response tests
  -- TODO: Add Request tests
