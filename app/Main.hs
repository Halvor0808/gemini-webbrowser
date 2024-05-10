module Main where

import Tui (tuiRun)
import Socket (sockTests)

main :: IO ()
main = do
  tuiRun >>= print

