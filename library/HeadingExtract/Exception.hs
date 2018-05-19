{-|
Module      : HeadingExtract.Exception
Description : Exceptions used during heading extraction.

Exceptions used during heading extraction.
-}
module HeadingExtract.Exception
  ( UnknownExtensionException(..)
  ) where

import           Control.Exception (Exception)

-- |Exception used when the file has an unknown extension.
newtype UnknownExtensionException =
  UnknownExtensionException String
  deriving (Eq)

instance Show UnknownExtensionException where
  show (UnknownExtensionException ext) = "Unknown extension " ++ ext

instance Exception UnknownExtensionException
