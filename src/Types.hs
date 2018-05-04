module Types where

import           Data.Text (Text)

type ResourcePath = Text

type Heading = Text

type IndexMatch = (ResourcePath, Heading)
