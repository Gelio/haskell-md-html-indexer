module Concurrent (mapConcurrently) where

import           System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkOS, ThreadId)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar, newMVar, MVar)

mapConcurrently :: (a -> IO ()) -> [a] -> IO ()
mapConcurrently f list = mapM_ (forkChild . f) list >> waitForChildren
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
