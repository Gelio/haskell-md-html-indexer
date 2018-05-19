{-|
Module      : HeadingExtract.Types
Description : Types used in headings extraction.

Types used in headings extraction.
-}
module HeadingExtract.Types where

import           Data.Text (Text)

-- |The path to the resource.
type ResourcePath = Text

-- |The heading in a resource.
type Heading = Text

-- |The matched resource along with one of its heading.
type IndexMatch = (ResourcePath, Heading)
