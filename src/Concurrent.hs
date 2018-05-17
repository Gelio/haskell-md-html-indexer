module Concurrent
  ( mapConcurrently
  ) where

import           Control.Concurrent      (ThreadId, forkOS)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          takeMVar)
import           System.IO.Unsafe        (unsafePerformIO)

-- Adapted from http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent.html#g:12
mapConcurrently :: Foldable t => (a -> IO ()) -> t a -> IO ()
mapConcurrently f list = mapM_ (forkChild . f) list >> waitForChildren
  where
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
      putMVar children (mvar : childs)
      forkOS (io >> putMVar mvar ())
