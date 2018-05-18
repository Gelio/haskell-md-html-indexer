module HeadingExtract.Internal.Markdown
  ( getHeadingsFromMarkdown
  , getHeadingsFromMarkdown'
  ) where

import           Conduit
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Text.Markdown         (defaultMarkdownSettings)
import           Text.Markdown.Block

import           HeadingExtract.Types

-- These functions are equivalent, just written using different syntax

-- |Parses a Markdown stream and yields the headings.
-- This function is equivalent to 'getHeadingsFromMarkdown'',
-- only the implementation is different.
getHeadingsFromMarkdown :: Monad m => ConduitT ByteString Heading m ()
getHeadingsFromMarkdown =
  decodeUtf8LenientC .| toBlocks defaultMarkdownSettings .| filterC isHeading .|
  mapC getHeadingText
  where
    isHeading :: Block Text -> Bool
    isHeading (BlockHeading _ _) = True
    isHeading _                  = False
    getHeadingText :: Block Text -> Text
    getHeadingText (BlockHeading _ text) = text
    getHeadingText _ = error "Cannot extract markdown heading text"

-- |Parses a Markdown stream and yields the headings.
-- This function is equivalent to 'getHeadingsFromMarkdown',
-- only the implementation is different.
getHeadingsFromMarkdown' :: Monad m => ConduitT ByteString Heading m ()
getHeadingsFromMarkdown' =
  decodeUtf8LenientC .| toBlocks defaultMarkdownSettings .| filterHeadingText
  where
    filterHeadingText :: Monad m => ConduitT (Block Text) Text m ()
    filterHeadingText = do
      mx <- await
      case mx of
        Nothing                    -> return ()
        Just (BlockHeading _ text) -> yield text >> filterHeadingText
        Just _                     -> filterHeadingText
