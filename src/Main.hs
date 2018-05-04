module Main where

import Conduit
import Data.Text (Text)

import IndexerOptions
import HeadingExtract

search :: SearchOptions -> FilePath -> IO ()
search _ _ = putStrLn "Search"

index :: IndexOptions -> FilePath -> IO ()
index _ _ = putStrLn "Index"

performCommand :: Options -> IO ()
performCommand (Options (Search o) path) = search o path
performCommand (Options (Index o) path) = index o path

main :: IO ()
main = parseOptions >>= performCommand
