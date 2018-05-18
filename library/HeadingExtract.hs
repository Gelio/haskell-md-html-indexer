module HeadingExtract
  ( getResourceHeadings
  , getResourceHeadingsTrimmed
  ) where

import           Conduit
import           Data.List              (isPrefixOf)
import           Data.Text              (strip)
import qualified Data.Text              as T

import           HeadingExtract.File
import           HeadingExtract.Network
import           HeadingExtract.Types

getResourceHeadings ::
     (MonadResource m, PrimMonad m, MonadThrow m)
  => String
  -> ConduitT i Heading m ()
getResourceHeadings s
  | isNetworkResource s = getNetworkHeadings s
  | otherwise = getFileHeadings s

isNetworkResource :: String -> Bool
isNetworkResource s = any (`isPrefixOf` s) ["http://", "https://"]

getResourceHeadingsTrimmed ::
     (PrimMonad m, MonadResource m, MonadThrow m)
  => String
  -> ConduitT i Heading m ()
getResourceHeadingsTrimmed s =
  getResourceHeadings s .| mapC strip .| filterC (not . T.null)
