-- Adapted from https://github.com/hspec/HUnit/blob/master/tests/HUnitTests.hs
module Main
  ( main
  ) where

import           System.Exit

import           Test.HUnit

import           Test.HeadingExtract.Internal.HTML (htmlTests)

tests :: Test
tests = TestList [htmlTests]

main :: IO ()
main = do
  counts2 <- runTestTT tests
  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
