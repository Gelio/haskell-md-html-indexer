module Search
  ( search
  ) where

import           Conduit
import           Control.Arrow     (second)
import qualified Data.Set          as Set
import           Data.Text         (pack, replace, unpack)
import qualified Data.Text         as T
import           System.Process    (callCommand)

import           Concurrent        (mapConcurrently)
import           IndexerOptions
import           Types

import           Control.Exception (SomeException, catch)

search :: SearchOptions -> FilePath -> IO ()
search (SearchOptions phrase cmd) input =
  catch
    (do matches <-
          runConduitRes $ readIndexFile input .| filterIndex phrase .| sinkList
        if null cmd
          then displayMatches matches
          else execMatches cmd matches)
    handler
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ "Cannot search the index file: " ++ show e

readIndexFile ::
     (MonadResource m, MonadThrow m) => FilePath -> ConduitT i IndexMatch m ()
readIndexFile path =
  sourceFile path .| decodeUtf8C .| linesUnboundedC .|
  mapC (T.break (== '\ETB')) .|
  mapC (second mapHeading)
  where
    mapHeading :: Heading -> Heading
    mapHeading h
      | T.null h = h
      | otherwise = T.tail h

filterIndex :: MonadResource m => String -> ConduitT IndexMatch IndexMatch m ()
filterIndex phrase = filterC ((phraseT `T.isInfixOf`) . snd)
  where
    phraseT = pack phrase

displayMatches :: [IndexMatch] -> IO ()
displayMatches matches =
  putStrLn ("Found " ++ show (length matches) ++ " matches:") >>
  mapM_ displayMatch matches

displayMatch :: IndexMatch -> IO ()
displayMatch (p, h) = putStrLn $ unpack p ++ ": " ++ unpack h

execMatches :: String -> [IndexMatch] -> IO ()
execMatches cmd matches = mapConcurrently execCmd paths
  where
    paths = Set.fromList $ fst <$> matches
    cmdT = pack cmd
    execCmd :: ResourcePath -> IO ()
    execCmd path =
      catch (callCommand $ unpack $ replace (pack "{}") path cmdT) handler
      where
        handler :: SomeException -> IO ()
        handler _ =
          putStrLn ("Cannot execute command on " ++ unpack path) >> putStrLn ""
        -- The actual error will be printed on the line above due to how callCommand works
