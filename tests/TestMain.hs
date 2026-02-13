module Main (main) where

import System.Exit
import Test.HUnit

import CBPV.Basic

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ basicTests
          ]
      )
  if errors counts2 + failures counts2 == 0 then
    exitSuccess
  else
    exitFailure
