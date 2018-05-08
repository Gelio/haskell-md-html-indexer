module Search
  ( search
  ) where

import           Conduit
import           Control.Arrow  (second)
import qualified Data.Set       as Set
import           Data.Text      (pack, replace, unpack)
import qualified Data.Text      as T
import           System.Process (callCommand)

import           IndexerOptions
import           Types

search :: SearchOptions -> FilePath -> IO ()
search (SearchOptions phrase cmd) input = do
  matches <-
    runConduitRes $ readIndexFile input .| filterIndex phrase .| sinkList
  if null cmd
    then displayMatches matches
    else execMatches cmd matches

readIndexFile ::
     (MonadResource m, MonadThrow m) => FilePath -> ConduitT i IndexMatch m ()
readIndexFile path =
  sourceFile path .| decodeUtf8C .| linesUnboundedC .|
  mapC (T.break (== '\ETB')) .|
  mapC (second T.tail)

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

-- TODO: possible concurrent execution
execMatches :: String -> [IndexMatch] -> IO ()
execMatches cmd matches = mapM_ execCmd paths
  where
    paths = Set.fromList $ fst <$> matches
    cmdT = pack cmd
    execCmd :: ResourcePath -> IO ()
    execCmd path = callCommand $ unpack $ replace (pack "{}") path cmdT
