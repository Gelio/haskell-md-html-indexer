module Test.HeadingExtract
  ( headingExtractTests
  ) where

import           Test.HUnit

import           HeadingExtract (isNetworkResource)

headingExtractTests :: Test
headingExtractTests =
  TestLabel "Headging extract tests" $ TestList [testsIsNetworkResource]

testsIsNetworkResource :: Test
testsIsNetworkResource =
  TestList
    [ TestCase
        (assertEqual "should mark http:// as network resource" True $
         isNetworkResource "http://example.com")
    , TestCase
        (assertEqual "should mark https:// as network resource" True $
         isNetworkResource "https://example.com")
    , TestCase
        (assertEqual "should mark regular file not as a network resource" False $
         isNetworkResource "regular-file.txt")
    , TestCase
        (assertEqual "should handle absolute file path" False $
         isNetworkResource "/regular-file.txt")
    ]
