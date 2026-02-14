module Main (main) where

import System.Exit
import Test.HUnit

import CBPV.Nat

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ natTests
          ]
      )
  if errors counts2 + failures counts2 == 0 then
    exitSuccess
  else
    exitFailure
