module HeadingExtract.Network
  ( getNetworkHeadings
  ) where

import           Conduit
import           Network.HTTP.Simple

import           HeadingExtract.Internal.HTML
import           HeadingExtract.Types

-- |Parses an online resource. It treats the resource as valid HTMl and yields the headings.
getNetworkHeadings ::
     (MonadResource m, MonadThrow m)
  => String -- ^ URL to the resource
  -> ConduitT i Heading m ()
getNetworkHeadings s =
  (parseRequest s >>= flip httpSource getResponseBody) .| getHeadingsFromHTML
