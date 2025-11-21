module Otus.Normalize.Result (
  EvalError (..),
  EvalResult,
  GuardedEvalResult,
  doPushVGSeg,
  doGetEnv,
  doAssignMeta,
  runGuardedResult,
) where

import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Otus.Ast
import Otus.Common
import Otus.Normalize.Env (Environment, GuardedEnvironment, assignMeta, fromEnv, intoEnv, pushGuarded)
import Otus.Normalize.Value (VGuardedSubstSeg, Value)

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | AppOnNonLambda
  | NatElimOnNonNat
  | JOnNonId
  | DBindOnNonDynamic
  | OpenNonLocal
  deriving (Eq, Show)

type EvalResult = Result EvalError

type GuardedEvalResult = StateT GuardedEnvironment EvalResult

doPushVGSeg :: VGuardedSubstSeg -> GuardedEvalResult ()
doPushVGSeg segVal = modify (`pushGuarded` segVal)

doGetEnv :: GuardedEvalResult Environment
doGetEnv = gets intoEnv

doAssignMeta :: LevelId -> Value -> GuardedEvalResult ()
doAssignMeta lvl val = modify (\e -> assignMeta e lvl val)

runGuardedResult :: GuardedEvalResult a -> Environment -> EvalResult (a, GuardedEnvironment)
runGuardedResult gRes = runStateT gRes . fromEnv
