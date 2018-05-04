module HeadingExtract.HTML (getHeadingsFromHTML) where

import Conduit
import Data.ByteString.Char8 (ByteString)
import Text.HTML.DOM
import Data.XML.Types
import Data.Text (unpack, Text)


getHeadingsFromHTML :: ConduitT ByteString o (ResourceT IO) ()
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
