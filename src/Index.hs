module Index
  ( index
  ) where

import           Conduit
import           Data.Text      (Text, pack, singleton)
import qualified Data.Text      as T
import           System.IO
import Control.Monad (when)

import Concurrent (mapConcurrently)
import           Exception      (handleIOError)
import           HeadingExtract
import           IndexerOptions (IndexOptions (..))

flushIndexedResults :: Bool
flushIndexedResults = True

-- TODO: append to index (as option)
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (\h -> mapConcurrently (safeIndexResource h) rs)
  putStrLn $ "Updated index at " ++ output

safeIndexResource :: Handle -> String -> IO ()
safeIndexResource h resource = handleIOError msg $ indexResource h resource
  where
    msg = "Cannot index resource " ++ resource

indexResource :: Handle -> String -> IO ()
indexResource handle resource = runConduitRes indexingConduit >> when flushIndexedResults (hFlush handle) >> putStrLn ("Finished indexing " ++ resource)
  where
    indexingConduit = getResourceHeadingsTrimmed resource .|
      mapC (insertResourcePath $ pack resource) .|
      unlinesC .|
      encodeUtf8C .|
      sinkHandle handle

insertResourcePath :: Text -> Text -> Text
insertResourcePath resource heading =
  T.concat [resource, singleton '\ETB', heading]
