module HeadingExtract.Network (getNetworkHeadings) where

import Conduit
import Network.HTTP.Simple
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

import HeadingExtract.Internal.HTML

-- Why won't it work?
-- getNetworkHeadings :: (MonadResource m, MonadThrow m) => String -> ConduitT i Text m ()
getNetworkHeadings :: String -> ConduitT i Text (ResourceT IO) ()
getNetworkHeadings s = (parseRequest s >>= flip httpSource getResponseBody) .| getHeadingsFromHTML
