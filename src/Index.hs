module Index (index) where

import Conduit
import Data.Text (Text, singleton, pack)
import qualified Data.Text as T
import System.IO

import IndexerOptions (IndexOptions(..))
import HeadingExtract
import Types


-- TODO: append to index (as option)
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (\h -> mapM_ (indexResource h) rs)
  putStrLn $ "Updated index at " ++ output

indexResource :: Handle -> String -> IO ()
indexResource handle resource = runConduitRes
  $ getResourceHeadingsTrimmed resource
  .| mapC (insertResourcePath $ pack resource)
  .| unlinesC
  .| encodeUtf8C
  .| sinkHandle handle

insertResourcePath :: Text -> Text -> Text
insertResourcePath resource heading = T.concat [resource, singleton '\ETB', heading]


