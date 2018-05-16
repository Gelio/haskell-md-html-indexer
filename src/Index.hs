module Index
  ( index
  ) where

import           Conduit
import           Data.Text      (Text, pack, singleton)
import qualified Data.Text      as T
import           System.IO
import qualified Control.Exception as X

import           HeadingExtract
import           IndexerOptions (IndexOptions (..))

-- TODO: append to index (as option)
-- TODO: index each file concurrently
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (\h -> mapM_ (safeIndexResource h) rs)
  putStrLn $ "Updated index at " ++ output

safeIndexResource :: Handle -> String -> IO ()
safeIndexResource h resource = X.handle errorHandler $ indexResource h resource
  where
    errorHandler :: X.SomeException -> IO ()
    errorHandler e = putStrLn $ "Cannot index resource " ++ resource ++ ": " ++ show e

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
