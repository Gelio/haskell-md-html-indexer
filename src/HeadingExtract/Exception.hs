module HeadingExtract.Exception (UnknownExtensionException(..)) where

import Control.Exception (Exception)

newtype UnknownExtensionException = UnknownExtensionException String

instance Show UnknownExtensionException where
  show (UnknownExtensionException ext) = "Unknown extension " ++ ext

instance Exception UnknownExtensionException
