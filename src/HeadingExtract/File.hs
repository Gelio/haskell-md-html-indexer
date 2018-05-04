module HeadingExtract.File (getFileHeadings) where

import Conduit
import Data.Conduit.Zlib (ungzip)
import System.FilePath.Posix (splitExtension)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Char (toLower)

import HeadingExtract.Internal.HTML
import HeadingExtract.Internal.Markdown

-- getFileHeadings :: MonadResource m => FilePath -> ConduitT i Text m () -- why won't it compile?
getFileHeadings :: FilePath -> ConduitT i Text (ResourceT IO) ()
getFileHeadings s = getFileHeadings' s s (mapC id)

getFileHeadings' :: FilePath -> String -> ConduitT ByteString ByteString (ResourceT IO) () -> ConduitT i Text (ResourceT IO) ()
getFileHeadings' fullPath s c
  | ext' == ".gz" = getFileHeadings' fullPath rest (c .| ungzip)
  | ext' `elem` [".html", ".htm"] = sourceFile fullPath .| c .| getHeadingsFromHTML
  | ext' == ".md" = sourceFile fullPath .| c .| getHeadingsFromMarkdown
  | otherwise = error "Unknown extension"
  where
    (rest, ext) = splitExtension s
    ext' = toLower <$> ext
