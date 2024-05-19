module Main where

import Tui (tuiRun)

main :: IO ()
main = do
  tuiRun >>= print

