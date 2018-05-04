module HeadingExtract.Markdown (getHeadingsFromMarkdown, getHeadingsFromMarkdown') where

import Conduit
import Data.ByteString.Char8 (ByteString)
import Text.Markdown.Block
import Text.Markdown (defaultMarkdownSettings)
import Data.Text (Text)

getHeadingsFromMarkdown :: ConduitT ByteString o (ResourceT IO) ()
getHeadingsFromMarkdown = decodeUtf8C .| toBlocks defaultMarkdownSettings .| filterC isHeading .| mapC getHeadingText .| mapM_C (lift . print)
  where
    isHeading :: Block Text -> Bool
    isHeading (BlockHeading _ _) = True
    isHeading _ = False

    getHeadingText :: Block Text -> Text
    getHeadingText (BlockHeading _ text) = text
    getHeadingText _ = error "Cannot extract markdown heading text"

getHeadingsFromMarkdown' :: ConduitT ByteString o (ResourceT IO) ()
getHeadingsFromMarkdown' = decodeUtf8C .| toBlocks defaultMarkdownSettings .| filterHeadingText .| mapM_C (lift . print)
  where
    filterHeadingText :: Monad m => ConduitT (Block Text) Text m ()
    filterHeadingText = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just (BlockHeading _ text) -> yield text >> filterHeadingText
        Just _ -> filterHeadingText
