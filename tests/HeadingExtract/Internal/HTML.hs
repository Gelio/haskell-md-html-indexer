module HeadingExtract.Internal.HTML (test1) where

import Conduit
import           Data.ByteString.Char8 (ByteString)
import Test.HUnit

import HeadingExtract.Internal.HTML (getHeadingsFromHTML)

getHeadingsFromHTML' :: String -> [Heading]
getHeadingsFromHTML' s = runConduitPure $ yield s .| getHeadingsFromHTML .| sinkList

test1 :: Test
test1 = TestCase (assertEqual ["heading"] $ getHeadingsFromHTML' "<h1>heading</h1>")
