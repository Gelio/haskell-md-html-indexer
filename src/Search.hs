module Search
  ( search
  ) where

import           Conduit
import qualified Data.Set       as Set
import           Data.Text      (Text, pack, replace, unpack)
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

readIndexFile :: FilePath -> ConduitT i IndexMatch (ResourceT IO) ()
readIndexFile path =
  sourceFile path .| decodeUtf8C .| linesUnboundedC .|
  mapC (T.break (== '\ETB')) .|
  mapC (\(res, headingWithETB) -> (res, T.tail headingWithETB))

filterIndex :: String -> ConduitT IndexMatch IndexMatch (ResourceT IO) ()
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
execMatches cmd matches = mapM_ (execCmd $ pack cmd) paths
  where
    paths = Set.fromList $ fst <$> matches
    execCmd :: Text -> ResourcePath -> IO ()
    execCmd cmd path = callCommand $ unpack $ replace (pack "{}") path cmd
