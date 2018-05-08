module HeadingExtract.Network
  ( getNetworkHeadings
  ) where

import           Conduit
import           Data.Text                    (Text)
import           Network.HTTP.Simple

import           HeadingExtract.Internal.HTML

-- Why won't it work?
getNetworkHeadings ::
     (MonadResource m, MonadThrow m) => String -> ConduitT i Text m ()
getNetworkHeadings s =
  (parseRequest s >>= flip httpSource getResponseBody) .| getHeadingsFromHTML
