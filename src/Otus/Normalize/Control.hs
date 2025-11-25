module Otus.Normalize.Control (
  EvalError (..),
  EvalResult,
  EvalResultT,
  EvalMonad,
  doPush,
  doPushFreshVar,
  doFind,
  doGetEnvLevel,
  returnAndPush,
  returnNeutral,
  runEvalMonad,
  evalEvalMonad,
) where

import Control.Monad.State.Strict (MonadState (put), StateT (runStateT), evalStateT, gets, modify)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env
import Otus.Normalize.Value

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | UnknownMeta LevelId
  | AppOnNonLambda
  | NatElimOnNonNat
  | JOnNonId
  | DBindOnNonDynamic
  | ForceOnNonLocal
  | AssignOnNonLocal
  | OpenNonLocal
  | UnsolvableTmEq VTelescope Value Value Value
  deriving (Eq, Show)

type EvalResult = Result EvalError

type EvalResultT = ResultT EvalError

type EvalMonad = StateT Environment EvalResult

doPush :: Value -> EvalMonad ()
doPush val = modify (push val)

returnAndPush :: Value -> EvalMonad Value
returnAndPush val = doPush val >> return val

doPushFreshVar :: EvalMonad Value
doPushFreshVar = do
  (val, env) <- gets pushFreshVar'
  put env
  return val

doFind :: (SeqIndex id) => id -> EvalMonad (Maybe Value)
doFind idx = gets $ find idx

doGetEnvLevel :: EvalMonad LevelId
doGetEnvLevel = gets envLevel

returnNeutral :: Neutral -> EvalResult Value
returnNeutral = return . VNeutral

runEvalMonad :: EvalMonad a -> Environment -> EvalResult (a, Environment)
runEvalMonad = runStateT

evalEvalMonad :: EvalMonad a -> Environment -> EvalResult a
evalEvalMonad = evalStateT
