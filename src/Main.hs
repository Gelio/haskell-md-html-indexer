module Main where

import IndexerOptions
import Network.HTTP.Simple
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Conduit
import Data.Conduit.Lift
import Data.Conduit.Zlib
import Conduit
import Control.Exception
import Text.HTML.DOM
import Data.XML.Types
import Data.List (isPrefixOf)
import Data.Text (unpack, Text)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as BS

search :: SearchOptions -> FilePath -> IO ()
search _ _ = putStrLn "Search"

getHeadersFromHTML :: ConduitT BS.ByteString o (ResourceT IO) ()
getHeadersFromHTML = eventConduit .| filterHeaders .| mapC getHeaderText .| mapM_C (lift . print)
  where
    filterHeaders = dropWhileC (not . isEventBeginHeader) >> headC >>= maybe (return ()) (const $ takeWhileC (not . isEventEndHeader) >> filterHeaders)

    getHeaderText :: Event -> Text
    getHeaderText (EventContent (ContentText x)) = x
    getHeaderText (EventContent (ContentEntity x)) = x
    getHeaderText _ = error "Cannot extract header text"

    isEventBeginElement :: Event -> Bool
    isEventBeginElement (EventBeginElement _ _) = True
    isEventBeginElement _ = False

    isEventEndElement :: Event -> Bool
    isEventEndElement (EventEndElement _) = True
    isEventEndElement _ = False

    isEventContent :: Event -> Bool
    isEventContent (EventContent _) = True
    isEventContent _ = False

    isElementHeader :: Name -> Bool
    isElementHeader (Name name _ _) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]

    isEventBeginHeader :: Event -> Bool
    isEventBeginHeader (EventBeginElement (Name name _ _) _) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]
    isEventBeginHeader _ = False

    isEventEndHeader :: Event -> Bool
    isEventEndHeader (EventEndElement (Name name _ _)) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]
    isEventEndHeader _ = False



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
