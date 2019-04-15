module Main where

import Config (config)
import Mattrai.CoreDataTypes
import Mattrai.MatTrai (runMatTrai)

main :: IO ()
main = runMatTrai config

