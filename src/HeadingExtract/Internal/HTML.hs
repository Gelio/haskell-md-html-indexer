module HeadingExtract.Internal.HTML
  ( getHeadingsFromHTML
  ) where

import           Conduit
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text, unpack)
import           Data.XML.Types
import           Text.HTML.DOM

getHeadingsFromHTML :: MonadResource m => ConduitT ByteString Text m ()
getHeadingsFromHTML = eventConduit .| filterHeadings .| getHeadingText

getHeadingText :: Monad m => ConduitT Event Text m ()
getHeadingText = do
  mx <- await
  case mx of
    Nothing                               -> return ()
    Just (EventContent (ContentText x))   -> yield x >> getHeadingText
    Just (EventContent (ContentEntity x)) -> yield x >> getHeadingText
    Just _                                -> getHeadingText

filterHeadings :: Monad m => ConduitT Event Event m ()
filterHeadings =
  dropWhileC (not . isEventBeginHeading) >> headC >>=
  maybe
    (return ())
    (const $ takeWhileC (not . isEventEndHeading) >> filterHeadings)
  where
    isEventBeginHeading :: Event -> Bool
    isEventBeginHeading (EventBeginElement (Name name _ _) _) =
      isHeading $ unpack name
    isEventBeginHeading _ = False
    isEventEndHeading :: Event -> Bool
    isEventEndHeading (EventEndElement (Name name _ _)) =
      isHeading $ unpack name
    isEventEndHeading _ = False

isHeading :: String -> Bool
isHeading = (`elem` ["h1", "h2", "h3", "h4", "h5", "h6"])
