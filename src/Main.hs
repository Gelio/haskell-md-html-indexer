module Main where

import IndexerOptions

main :: IO ()
main = parseOptions >>= print
