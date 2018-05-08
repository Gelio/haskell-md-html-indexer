module HeadingExtract.File
  ( getFileHeadings
  ) where

import           Conduit
import           Data.ByteString.Char8            (ByteString)
import           Data.Char                        (toLower)
import           Data.Conduit.Zlib                (ungzip)
import           Data.Text                        (Text)
import           System.FilePath.Posix            (splitExtension)

import           HeadingExtract.Internal.HTML
import           HeadingExtract.Internal.Markdown

getFileHeadings :: (MonadResource m, PrimMonad m, MonadThrow m) => FilePath -> ConduitT i Text m ()
getFileHeadings s = getFileHeadings' s s (mapC id)

getFileHeadings' :: (MonadResource m, PrimMonad m, MonadThrow m) =>
     FilePath
  -> String
  -> ConduitT ByteString ByteString m ()
  -> ConduitT i Text m ()
getFileHeadings' fullPath s c
  | ext' == ".gz" = getFileHeadings' fullPath rest (c .| ungzip)
  | ext' `elem` [".html", ".htm"] =
    sourceFile fullPath .| c .| getHeadingsFromHTML
  | ext' == ".md" = sourceFile fullPath .| c .| getHeadingsFromMarkdown
  | otherwise = error "Unknown extension"
  where
    (rest, ext) = splitExtension s
    ext' = toLower <$> ext
