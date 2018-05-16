module HeadingExtract.Exception (UnknownExtensionException(..)) where

import Control.Exception (Exception)

newtype UnknownExtensionException = UnknownExtensionException String deriving Show

instance Exception UnknownExtensionException
