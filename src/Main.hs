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
import Text.Markdown (defaultMarkdownSettings)
import qualified Text.Markdown.Block as MD
import qualified Data.ByteString.Char8 as BS

search :: SearchOptions -> FilePath -> IO ()
search _ _ = putStrLn "Search"

getHeadingsFromHTML :: ConduitT BS.ByteString o (ResourceT IO) ()
getHeadingsFromHTML = eventConduit .| filterHeaders .| mapC getHeadingText .| mapM_C (lift . print)
  where
    filterHeaders = dropWhileC (not . isEventBeginHeading) >> headC >>= maybe (return ()) (const $ takeWhileC (not . isEventEndHeading) >> filterHeaders)

    getHeadingText :: Event -> Text
    getHeadingText (EventContent (ContentText x)) = x
    getHeadingText (EventContent (ContentEntity x)) = x
    getHeadingText _ = error "Cannot extract HTML heading text"

    isEventBeginElement :: Event -> Bool
    isEventBeginElement (EventBeginElement _ _) = True
    isEventBeginElement _ = False

    isEventEndElement :: Event -> Bool
    isEventEndElement (EventEndElement _) = True
    isEventEndElement _ = False

    isEventContent :: Event -> Bool
    isEventContent (EventContent _) = True
    isEventContent _ = False

    isElementHeading :: Name -> Bool
    isElementHeading (Name name _ _) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]

    isEventBeginHeading :: Event -> Bool
    isEventBeginHeading (EventBeginElement (Name name _ _) _) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]
    isEventBeginHeading _ = False

    isEventEndHeading :: Event -> Bool
    isEventEndHeading (EventEndElement (Name name _ _)) = unpack name `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]
    isEventEndHeading _ = False

getHeadingsFromMarkdown :: ConduitT BS.ByteString o (ResourceT IO) ()
getHeadingsFromMarkdown = decodeUtf8C .| MD.toBlocks defaultMarkdownSettings .| filterC isHeading .| mapC getHeadingText .| mapM_C (lift . print)
  where
    isHeading :: MD.Block Text -> Bool
    isHeading (MD.BlockHeading _ _) = True
    isHeading _ = False

    getHeadingText :: MD.Block Text -> Text
    getHeadingText (MD.BlockHeading _ text) = text
    getHeadingText _ = error "Cannot extract markdown heading text"

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
