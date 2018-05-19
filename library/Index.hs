{-|
Module      : Index
Description : Retrieves the headings from supported resources and saves them in the index (file).

Retrieves the headings from supported resources and saves them in the index (file).
-}
module Index
  ( index
  ) where

import           Conduit
import           Control.Monad  (when)
import           Data.Text      (Text, pack, singleton)
import qualified Data.Text      as T
import           System.IO

import           Concurrent     (mapConcurrently)
import           Exception      (handleIOError)
import           HeadingExtract
import           IndexerOptions (IndexOptions (..))
import           HeadingExtract.Types

flushIndexedResults :: Bool
flushIndexedResults = True

-- TODO: append to index (as option)
-- |Indexes HTML and Markdown resources and saves the index to a specified file.
-- Handles:
-- * HTML network resources
-- * HTML/Markdown from the local filesystem (also gzipped)
--
-- Each resource is indexed concurrently and using streams.
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (\h -> mapConcurrently (safeIndexResource h) rs)
  putStrLn $ "Updated index at " ++ output

-- |Indexes a single resource. Handles any exceptions.
safeIndexResource :: Handle -> String -> IO ()
safeIndexResource h resource = handleIOError msg $ indexResource h resource
  where
    msg = "Cannot index resource " ++ resource

-- |Indexes a single resource. Doen not handle exceptions.
indexResource :: Handle -> String -> IO ()
indexResource handle resource =
  runConduitRes indexingConduit >> when flushIndexedResults (hFlush handle) >>
  putStrLn ("Finished indexing " ++ resource)
  where
    indexingConduit =
      getResourceHeadingsTrimmed resource .|
      mapC (insertResourcePath $ pack resource) .|
      unlinesC .|
      encodeUtf8C .|
      sinkHandle handle

-- |Inserts a resource path into the heading line.
insertResourcePath :: ResourcePath -> Heading -> Text
insertResourcePath resource heading =
  T.concat [resource, singleton '\ETB', heading]
