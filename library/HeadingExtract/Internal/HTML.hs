{-|
Module      : HeadingExtract.Internal.HTML
Description : Extracts headings from HTML.

Extracts headings from HTML.
-}
module HeadingExtract.Internal.HTML
  ( getHeadingsFromHTML
  ) where

import           Conduit
import           Data.ByteString.Char8 (ByteString)
import           Data.Char             (toLower)
import           Data.Text             (unpack)
import           Data.XML.Types
import           Text.HTML.DOM

import           HeadingExtract.Types

-- |Parses a HTML stream and yields the headings
getHeadingsFromHTML :: Monad m => ConduitT ByteString Heading m ()
getHeadingsFromHTML = eventConduit .| filterHeadings .| getHeadingText

-- |Retrieves the heading text from a HTML parsing event
getHeadingText :: Monad m => ConduitT Event Heading m ()
getHeadingText = do
  mx <- await
  case mx of
    Nothing                               -> return ()
    Just (EventContent (ContentText x))   -> yield x >> getHeadingText
    Just (EventContent (ContentEntity x)) -> yield x >> getHeadingText
    Just _                                -> getHeadingText

-- |Filters the HTML events and yields elements/text inside headings
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

-- |Tests whether the argument is a valid heading tag
isHeading :: String -> Bool
isHeading = (`elem` ["h1", "h2", "h3", "h4", "h5", "h6"]) . map toLower
