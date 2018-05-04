module Main where

import Conduit
import Control.Monad
import Data.Text (Text, singleton, pack)
import qualified Data.Text as T
import System.IO

import IndexerOptions
import HeadingExtract

search :: SearchOptions -> FilePath -> IO ()
search (SearchOptions phrase exec) input = putStrLn "Search"

index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = withFile output WriteMode (\h -> mapM_ (indexResource h) rs)
  where
    indexResource :: Handle -> String -> IO ()
    indexResource handle resource = runConduitRes
      $ getResourceHeadingsTrimmed resource
      .| mapC (insertResourcePath $ pack resource)
      .| unlinesC
      .| encodeUtf8C
      .| sinkHandle handle

    insertResourcePath :: Text -> Text -> Text
    insertResourcePath resource heading = T.concat [resource, singleton '\ETB', heading]

performCommand :: Options -> IO ()
performCommand (Options (Search o) path) = search o path
performCommand (Options (Index o) path) = index o path

main :: IO ()
main = parseOptions >>= performCommand
