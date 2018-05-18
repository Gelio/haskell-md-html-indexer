module Test.HeadingExtract.Internal.Markdown
  ( markdownTests
  ) where

import           Conduit
import qualified Data.ByteString.Char8            as BS
import qualified Data.Text                        as T
import           Test.HUnit

import           HeadingExtract.Internal.Markdown (getHeadingsFromMarkdown)

getHeadingsFromMarkdown' :: String -> [String]
getHeadingsFromMarkdown' s =
  runConduitPure $
  yield s' .| getHeadingsFromMarkdown .| mapC T.unpack .| sinkList
  where
    s' = BS.pack s

markdownTests :: Test
markdownTests =
  TestLabel "Markdown parsing tests" $
  TestList
    [ testsSingleHeading
    , testAlternativeHeadings
    , testMultipleHeadings
    , testMultipleHeadings'
    , testHeadingWithOtherElements
    ]

possibleHeadings :: [String]
possibleHeadings = ["#", "##", "###", "####", "#####", "######"]

testsSingleHeading :: Test
testsSingleHeading = TestList $ map singleTest possibleHeadings
  where
    singleTest :: String -> Test
    singleTest h =
      TestCase (assertEqual label ["heading"] $ getHeadingsFromMarkdown' md)
      where
        label = "should parse a single " ++ h ++ " heading"
        md = h ++ " heading"

testAlternativeHeadings :: Test
testAlternativeHeadings =
  TestList
    [ TestCase
        (assertEqual "should parse a --- alternative heading" ["heading"] $
         getHeadingsFromMarkdown' "heading\n---")
    , TestCase
        (assertEqual "should parse a === alternative heading" ["heading"] $
         getHeadingsFromMarkdown' "heading\n===")
    ]

testMultipleHeadings :: Test
testMultipleHeadings =
  TestCase
    (assertEqual "should parse multiple headings" ["heading1", "heading2"] $
     getHeadingsFromMarkdown' "# heading1\n## heading2")

testMultipleHeadings' :: Test
testMultipleHeadings' =
  TestCase
    (assertEqual
       "should parse multiple headings with content in between"
       ["heading1", "heading2"] $
     getHeadingsFromMarkdown' "# heading1\n\nsome content\n\n## heading2")

testHeadingWithOtherElements :: Test
testHeadingWithOtherElements =
  TestCase
    (assertEqual "should allow other Markdown elements" ["heading1", "heading2"] $
     getHeadingsFromMarkdown'
       "# heading1\n\n* list\n* other element\n\n1. List\n2. List2\n\n## heading2\n\n```bash\nsome code\n```")
