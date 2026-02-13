module CBPV.Helper (assertCheck) where

import Test.HUnit

import Otus

assertCheck :: String -> MetaTerm -> MetaTerm -> Assertion
assertCheck name preTm preTy =
  let split = "\n----------------------" ++ name ++ "-----------------------\n"
  in case inferComputationTy emptyCtx preTy of
       Success (WfComputation cty _ _, _) ->
         let checkTy =
               doEvalComputation cty emptyEnv
                 >>= \vcty -> checkComputation emptyCtx preTm vcty
         in case checkTy of
              Success (WfComputation t eff _) ->
                putStrLn
                  ( split
                      ++ "Infer: \n  "
                      ++ pretty t
                      ++ "\n"
                      ++ "As: \n "
                      ++ pretty eff
                      ++ " !! "
                      ++ pretty cty
                      ++ "\n"
                  )
              Failure e ->
                assertFailure
                  ( split
                      ++ "Error: \n  "
                      ++ show e
                      ++ "\nwhile checking computation: \n  "
                      ++ pretty preTm
                      ++ "\nAs: \n  "
                      ++ pretty cty
                      ++ "\n"
                  )
       Failure e ->
         assertFailure
           ( split
               ++ "Error: \n    "
               ++ show e
               ++ "\nwhile infering computation type: \n   "
               ++ pretty preTy
               ++ "\n"
           )
