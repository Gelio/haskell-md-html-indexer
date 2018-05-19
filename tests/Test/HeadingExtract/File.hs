module Test.HeadingExtract.File
  ( fileHeadingExtractTests
  ) where

import           Conduit
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8    as BS
import           Data.Conduit.Zlib        (gzip)
import qualified Data.Text                as T
import           Test.HUnit

import           HeadingExtract.Exception
import           HeadingExtract.File      (createHeadingExtractionConduit)
import           HeadingExtract.Types

fileHeadingExtractTests :: Test
fileHeadingExtractTests =
  TestLabel "File heading extract tests" $
  TestList
    [ testMarkdownHeadingExtraction
    , testHTMLHeadingExtraction
    , testHTMHeadingExtraction
    , testGzippedHTMLHeadingExtraction
    , testUnknownException
    ]

getHeadingsFromConduit ::
     String -> ConduitT BS.ByteString Heading IO () -> IO [String]
getHeadingsFromConduit s c =
  runConduit $ yield s' .| c .| mapC T.unpack .| sinkList
  where
    s' = BS.pack s

-- Source: https://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $
  action >> (assertFailure $ "Expected exception: " ++ show ex)
  where
    isWanted = guard . (== ex)

testMarkdownHeadingExtraction :: Test
testMarkdownHeadingExtraction =
  TestCase
    (getHeadingsFromConduit
       "# heading\n\ntext"
       (createHeadingExtractionConduit "anything.md") >>= \headings ->
       assertEqual "should detect Markdown file" ["heading"] headings)

testHTMLHeadingExtraction :: Test
testHTMLHeadingExtraction =
  TestCase
    (getHeadingsFromConduit
       "<h1>heading</h1>"
       (createHeadingExtractionConduit "index.html") >>= \headings ->
       assertEqual "should detect HTML file" ["heading"] headings)

testHTMHeadingExtraction :: Test
testHTMHeadingExtraction =
  TestCase
    (getHeadingsFromConduit
       "<h1>heading</h1>"
       (createHeadingExtractionConduit "index.htm") >>= \headings ->
       assertEqual "should detect HTML file" ["heading"] headings)

testGzippedHTMLHeadingExtraction :: Test
testGzippedHTMLHeadingExtraction =
  TestCase
    (getHeadingsFromConduit
       "<h1>heading</h1>"
       (gzip .| createHeadingExtractionConduit "index.html.gz") >>= \headings ->
       assertEqual "should detect gzipped HTML file" ["heading"] headings)

testUnknownException :: Test
testUnknownException =
  TestCase
    (assertException
       (UnknownExtensionException ".unknown")
       (getHeadingsFromConduit
          "<h1>heading</h1>"
          (createHeadingExtractionConduit "index.html.gz.unknown")))
