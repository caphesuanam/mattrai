module Main where

import Config (config)
import CoreDataTypes
import MatTrai (runMatTrai)

main :: IO ()
main = runMatTrai config

