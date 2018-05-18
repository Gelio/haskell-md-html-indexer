module Test.HeadingExtract.Internal.HTML
  ( htmlTests
  ) where

import           Conduit
import qualified Data.ByteString.Char8        as BS
import qualified Data.Text                    as T
import           Test.HUnit

import           HeadingExtract.Internal.HTML (getHeadingsFromHTML)

getHeadingsFromHTML' :: String -> [String]
getHeadingsFromHTML' s =
  runConduitPure $ yield s' .| getHeadingsFromHTML .| mapC T.unpack .| sinkList
  where
    s' = BS.pack s

htmlTests :: Test
htmlTests =
  TestLabel "HTML parsing tests" $
  TestList
    [ testsSingleHeading
    , testMultipleHeadings
    , testMultipleHeadings'
    , testHTMLInsideHeading
    , testUnclosedHeading
    , testHeadingInsideOtherTags
    , testHeadingCase
    ]

possibleHeadings :: [String]
possibleHeadings = ["h1", "h2", "h3", "h4", "h5", "h6"]

testsSingleHeading :: Test
testsSingleHeading = TestList $ map singleTest possibleHeadings
  where
    singleTest :: String -> Test
    singleTest h =
      TestCase (assertEqual label ["heading"] $ getHeadingsFromHTML' html)
      where
        label = "should parse a single " ++ h ++ " heading"
        html = "<" ++ h ++ ">heading</" ++ h ++ ">"

testMultipleHeadings :: Test
testMultipleHeadings =
  TestCase
    (assertEqual "should parse multiple headings" ["heading1", "heading2"] $
     getHeadingsFromHTML' "<h1>heading1</h1><h2>heading2</h2>")

testMultipleHeadings' :: Test
testMultipleHeadings' =
  TestCase
    (assertEqual
       "should parse multiple headings with content in between"
       ["heading1", "heading2"] $
     getHeadingsFromHTML' "<h1>heading1</h1>some other content<h2>heading2</h2>")

testHTMLInsideHeading :: Test
testHTMLInsideHeading =
  TestCase
    (assertEqual "should accept HTML inside headings" ["heading ", "with html"] $
     getHeadingsFromHTML' "<h1>heading <span>with html</span></h1>")

testUnclosedHeading :: Test
testUnclosedHeading =
  TestCase
    (assertEqual "should index unfinished headings" ["heading"] $
     getHeadingsFromHTML' "<h1>heading")

testHeadingInsideOtherTags :: Test
testHeadingInsideOtherTags =
  TestCase
    (assertEqual "should index headings inside other tags" ["heading"] $
     getHeadingsFromHTML'
       "<html><head></head><body><main><h1>heading</h1></main></body></html>")

testHeadingCase :: Test
testHeadingCase =
  TestCase
    (assertEqual
       "should index headings regardless of the character case"
       ["heading", "heading2", "heading3"] $
     getHeadingsFromHTML' "<H1>heading</h1><h2>heading2</H2><H3>heading3</H3>")
