{-|
Module      : HeadingExtract.File
Description : Retrieves the headings from resources from the local filesystem.

Retrieves the headings from resources from the local filesystem.

Handles HTML/Markdown (with gzip) from the local filesystem.
-}
module HeadingExtract.File
  ( getFileHeadings
  ) where

import           Conduit
import           Data.ByteString.Char8            (ByteString)
import           Data.Char                        (toLower)
import           Data.Conduit.Zlib                (ungzip)
import           System.FilePath.Posix            (splitExtension)

import           HeadingExtract.Exception         (UnknownExtensionException (..))
import           HeadingExtract.Internal.HTML
import           HeadingExtract.Internal.Markdown
import           HeadingExtract.Types

-- |Gets the headings from a file. Handles gzipped Markdown and html files.
getFileHeadings ::
     (MonadResource m, PrimMonad m, MonadThrow m)
  => FilePath -- ^ Path to the file
  -> ConduitT i Heading m ()
getFileHeadings s = getFileHeadings' s s (mapC id)

-- |Used internally to construct the proper conduits to read and parse the file.
getFileHeadings' ::
     (MonadResource m, PrimMonad m, MonadThrow m)
  => FilePath -- ^ Full file path
  -> String -- ^ Leftover file path. Further execution depends on this path's extension
  -> ConduitT ByteString ByteString m () -- ^ The parsing conduit constructed so far
  -> ConduitT i Heading m ()
getFileHeadings' fullPath s c
  | ext' == ".gz" = getFileHeadings' fullPath rest (c .| ungzip)
  | ext' `elem` [".html", ".htm"] =
    sourceFile fullPath .| c .| getHeadingsFromHTML
  | ext' == ".md" = sourceFile fullPath .| c .| getHeadingsFromMarkdown
  | otherwise = throwM $ UnknownExtensionException ext'
  where
    (rest, ext) = splitExtension s
    ext' = toLower <$> ext
