{-|
Module      : HeadingExtract.File
Description : Retrieves the headings from resources from the local filesystem.

Retrieves the headings from resources from the local filesystem.

Handles HTML/Markdown (with gzip) from the local filesystem.
-}
module HeadingExtract.File
  ( getFileHeadings
  , createHeadingExtractionConduit
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
getFileHeadings s = sourceFile s .| createHeadingExtractionConduit s

-- |Constructs the proper conduits to parse the file and extract the headings.
createHeadingExtractionConduit ::
     (PrimMonad m, MonadThrow m)
  => String -- ^ File path
  -> ConduitT ByteString Heading m ()
createHeadingExtractionConduit = createHeadingExtractionConduit' idConduit
  where
    idConduit = mapC id

-- |Constructs the proper conduits to parse the file and extract the headings.
createHeadingExtractionConduit' ::
     (PrimMonad m, MonadThrow m)
  => ConduitT ByteString ByteString m () -- ^ The parsing conduit constructed so far
  -> String -- ^ Leftover file path. Further parsing depends on this path's extension
  -> ConduitT ByteString Heading m ()
createHeadingExtractionConduit' c s
  | ext' == ".gz" = createHeadingExtractionConduit' (c .| ungzip) rest
  | ext' `elem` [".html", ".htm"] = c .| getHeadingsFromHTML
  | ext' == ".md" = c .| getHeadingsFromMarkdown
  | otherwise = throwM $ UnknownExtensionException ext'
  where
    (rest, ext) = splitExtension s
    ext' = toLower <$> ext
