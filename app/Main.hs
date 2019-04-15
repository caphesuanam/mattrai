module Main where

import Config (config)
import Mattrai.Service
import Mattrai (runMattrai)

main :: IO ()
main = runMattrai config

