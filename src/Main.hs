module Main where

import Network.HTTP.Simple
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Conduit
import Data.Conduit.Lift
import Data.Conduit.Zlib
import Conduit
import Control.Exception
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as BS

import IndexerOptions
import HeadingExtract.HTML
import HeadingExtract.Markdown

search :: SearchOptions -> FilePath -> IO ()
search _ _ = putStrLn "Search"

getSource :: (PrimMonad m, MonadResource m, MonadThrow m) => String -> ConduitT i BS.ByteString m ()
getSource s
  | "http://" `isPrefixOf` s = getHttpSource s
  | otherwise = getFileSource s

getHttpSource :: (MonadResource m, MonadThrow m) => String -> ConduitT i BS.ByteString m ()
getHttpSource s = parseRequest s >>= flip httpSource getResponseBody

getFileSource :: MonadResource m => FilePath -> ConduitT i BS.ByteString m ()
getFileSource = sourceFile

getUngzipFileSource :: (PrimMonad m, MonadResource m, MonadThrow m) => FilePath -> ConduitT i BS.ByteString m ()
getUngzipFileSource s = getFileSource s .| ungzip

index :: IndexOptions -> FilePath -> IO ()
index _ _ = putStrLn "Index"

performCommand :: Options -> IO ()
performCommand (Options (Search o) path) = search o path
performCommand (Options (Index o) path) = index o path

main :: IO ()
main = parseOptions >>= performCommand
