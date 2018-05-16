module Index
  ( index
  ) where

import           Conduit
import           Data.Text      (Text, pack, singleton)
import qualified Data.Text      as T
import           System.IO

import           HeadingExtract
import Exception (handleIOError)
import           IndexerOptions (IndexOptions (..))

-- TODO: append to index (as option)
-- TODO: index each file concurrently
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (\h -> mapM_ (safeIndexResource h) rs)
  putStrLn $ "Updated index at " ++ output

safeIndexResource :: Handle -> String -> IO ()
safeIndexResource h resource = handleIOError msg $ indexResource h resource
  where
    msg = "Cannot index resource " ++ resource

indexResource :: Handle -> String -> IO ()
indexResource handle resource =
  runConduitRes $
  getResourceHeadingsTrimmed resource .|
  mapC (insertResourcePath $ pack resource) .|
  unlinesC .|
  encodeUtf8C .|
  sinkHandle handle

insertResourcePath :: Text -> Text -> Text
insertResourcePath resource heading =
  T.concat [resource, singleton '\ETB', heading]
