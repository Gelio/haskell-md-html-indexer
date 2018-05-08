module HeadingExtract.Internal.Markdown
  ( getHeadingsFromMarkdown
  , getHeadingsFromMarkdown'
  ) where

import           Conduit
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Text.Markdown         (defaultMarkdownSettings)
import           Text.Markdown.Block

getHeadingsFromMarkdown ::
     (MonadResource m, MonadThrow m) => ConduitT ByteString Text m ()
getHeadingsFromMarkdown =
  decodeUtf8C .| toBlocks defaultMarkdownSettings .| filterC isHeading .|
  mapC getHeadingText
  where
    isHeading :: Block Text -> Bool
    isHeading (BlockHeading _ _) = True
    isHeading _                  = False
    getHeadingText :: Block Text -> Text
    getHeadingText (BlockHeading _ text) = text
    getHeadingText _ = error "Cannot extract markdown heading text"

getHeadingsFromMarkdown' ::
     (MonadResource m, MonadThrow m) => ConduitT ByteString Text m ()
getHeadingsFromMarkdown' =
  decodeUtf8C .| toBlocks defaultMarkdownSettings .| filterHeadingText
  where
    filterHeadingText :: Monad m => ConduitT (Block Text) Text m ()
    filterHeadingText = do
      mx <- await
      case mx of
        Nothing                    -> return ()
        Just (BlockHeading _ text) -> yield text >> filterHeadingText
        Just _                     -> filterHeadingText
