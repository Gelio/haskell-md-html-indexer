module HeadingExtract
  ( getResourceHeadings
  , getResourceHeadingsTrimmed
  ) where

import           Conduit
import           Data.List              (isPrefixOf)
import           Data.Text              (Text, strip)
import qualified Data.Text              as T

import           HeadingExtract.File
import           HeadingExtract.Network

-- How can I not specify exactly ResourceT IO as the monad?
-- getResourceHeadings :: String -> ConduitT i Text (ResourceT IO) ()
getResourceHeadings :: String -> ConduitT i Text (ResourceT IO) ()
getResourceHeadings s
  | isNetworkResource s = getNetworkHeadings s
  | otherwise = getFileHeadings s

isNetworkResource :: String -> Bool
isNetworkResource s = any (`isPrefixOf` s) ["http://", "https://"]

-- Why won't it work?
-- getResourceHeadingsTrimmed :: (PrimMonad m, MonadResource m, MonadThrow m) => String -> ConduitT i Text m ()
getResourceHeadingsTrimmed :: String -> ConduitT i Text (ResourceT IO) ()
getResourceHeadingsTrimmed s =
  getResourceHeadings s .| mapC strip .| filterC (not . T.null)
