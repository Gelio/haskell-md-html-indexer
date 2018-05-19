{-|
Module      : Main
Description : The main module for the executable.

The main module for the executable.
-}
module Main where

import           Index          (index)
import           IndexerOptions
import           Search         (search)

-- |Executes the executable command
performCommand :: Options -> IO ()
performCommand (Options (Search o) path) = search o path
performCommand (Options (Index o) path)  = index o path

main :: IO ()
main = parseOptions >>= performCommand
