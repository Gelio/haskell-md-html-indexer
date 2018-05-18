module HeadingExtract.Network
  ( getNetworkHeadings
  ) where

import           Conduit
import           Network.HTTP.Simple

import           HeadingExtract.Internal.HTML
import           HeadingExtract.Types

getNetworkHeadings ::
     (MonadResource m, MonadThrow m) => String -> ConduitT i Heading m ()
getNetworkHeadings s =
  (parseRequest s >>= flip httpSource getResponseBody) .| getHeadingsFromHTML
