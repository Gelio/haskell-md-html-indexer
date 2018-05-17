module Index
  ( index
  ) where

import           Conduit
import           Data.Text      (Text, pack, singleton)
import qualified Data.Text      as T
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import Control.Concurrent (forkOS, ThreadId)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar, newMVar, MVar)

import           Exception      (handleIOError)
import           HeadingExtract
import           IndexerOptions (IndexOptions (..))

flushIndexedResults :: Bool
flushIndexedResults = True

-- TODO: append to index (as option)
index :: IndexOptions -> FilePath -> IO ()
index (IndexOptions rs) output = do
  withFile output WriteMode (concurrentlyIndexResources rs)
  putStrLn $ "Updated index at " ++ output

concurrentlyIndexResources :: [String] -> Handle ->IO ()
concurrentlyIndexResources rs h = do
  mapM_ (forkChild . safeIndexResource h) rs
  waitForChildren

  where
    -- Adapted from http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent.html#g:12
    children :: MVar [MVar ()]
    children = unsafePerformIO (newMVar [])

    waitForChildren :: IO ()
    waitForChildren = do
      cs <- takeMVar children
      case cs of
        [] -> return ()
        m:ms -> do
          putMVar children ms
          takeMVar m
          waitForChildren

    forkChild :: IO () -> IO ThreadId
    forkChild io = do
      mvar <- newEmptyMVar
      childs <- takeMVar children
      putMVar children (mvar:childs)
      forkOS (io >> putMVar mvar ())

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
