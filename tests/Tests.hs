-- Adapted from https://github.com/hspec/HUnit/blob/master/tests/HUnitTests.hs
module Main
  ( main
  ) where

import           System.Exit

import           Test.HUnit

import           Test.HeadingExtract                   (headingExtractTests)
import           Test.HeadingExtract.File              (fileHeadingExtractTests)
import           Test.HeadingExtract.Internal.HTML     (htmlTests)
import           Test.HeadingExtract.Internal.Markdown (markdownTests)

tests :: Test
tests =
  TestList
    [htmlTests, markdownTests, headingExtractTests, fileHeadingExtractTests]

main :: IO ()
main = do
  counts2 <- runTestTT tests
  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
