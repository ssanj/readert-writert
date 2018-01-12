module Main where

import Config (readWriteConfig)
import People (readWritePeople)

main :: IO ()
main = do
  _ <- readWritePeople
  readWriteConfig