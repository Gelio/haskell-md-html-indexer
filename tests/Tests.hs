-- Adapted from https://github.com/hspec/HUnit/blob/master/tests/HUnitTests.hs
module Main
  ( main
  ) where

import           System.Exit
import           Test.HUnit

import HeadingExtract.Internal.HTML

tests :: Test
tests = TestList [test1]

main :: IO ()
main = do
  counts2 <- runTestTT tests
  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
