{-|
Module      : HeadingExtract
Description : Retrieves the headings from supported resources.

Retrieves the headings from supported resources.

Handles HTML via network and HTML/Markdown (with gzip) from the local filesystem.
-}
module HeadingExtract
  ( getResourceHeadings
  , getResourceHeadingsTrimmed
  , isNetworkResource
  ) where

import           Conduit
import           Data.List              (isPrefixOf)
import           Data.Text              (strip)
import qualified Data.Text              as T

import           HeadingExtract.File
import           HeadingExtract.Network
import           HeadingExtract.Types

-- |Parses the resource and retrieves the headings. Handles HTML via network
-- and HTML/Markdown (with gzip) from the local filesystem.
getResourceHeadings ::
     (MonadResource m, PrimMonad m, MonadThrow m)
  => String -- ^ Path to the resource
  -> ConduitT i Heading m ()
getResourceHeadings s
  | isNetworkResource s = getNetworkHeadings s
  | otherwise = getFileHeadings s

-- |Determines if the resource should be fetched from the Internet.
isNetworkResource :: String -> Bool
isNetworkResource s = any (`isPrefixOf` s) ["http://", "https://"]

-- |Does the same as 'getResourceHeadings' but returns only not empty
-- headings.
getResourceHeadingsTrimmed ::
     (PrimMonad m, MonadResource m, MonadThrow m)
  => String
  -> ConduitT i Heading m ()
getResourceHeadingsTrimmed s =
  getResourceHeadings s .| mapC strip .| filterC (not . T.null)
