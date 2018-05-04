module Main where

import           Index          (index)
import           IndexerOptions
import           Search         (search)

performCommand :: Options -> IO ()
performCommand (Options (Search o) path) = search o path
performCommand (Options (Index o) path)  = index o path

main :: IO ()
main = parseOptions >>= performCommand
