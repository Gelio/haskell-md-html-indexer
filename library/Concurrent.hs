{-|
Module      : Concurrent
Description : Concurrent helper functions

Concurrent helper functions
-}
module Concurrent
  ( mapConcurrently
  ) where

import           Control.Concurrent      (ThreadId, forkOS)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          takeMVar)

-- Adapted from http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent.html#g:12
-- |Maps each item concurrently. A separate OS thread is used for each element.
-- This functions wait for all created threads to finish before ending.
mapConcurrently ::
     Foldable t
  => (a -> IO ()) -- ^ The mapping function
  -> t a -- ^ Elements to map over
  -> IO ()
mapConcurrently f list = do
  children <- newMVar []
  mapM_ (forkChild children . f) list >> waitForChildren children
  where
    waitForChildren :: MVar [MVar ()] -> IO ()
    waitForChildren children = do
      cs <- takeMVar children
      case cs of
        [] -> return ()
        m:ms -> do
          putMVar children ms
          takeMVar m
          waitForChildren children
    forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
    forkChild children io = do
      mvar <- newEmptyMVar
      childs <- takeMVar children
      putMVar children (mvar : childs)
      forkOS (io >> putMVar mvar ())
