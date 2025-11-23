module Otus.Normalize.Control (
  EvalError (..),
  EvalResult,
  EvalMonad,
  doPush,
  doPushMetaView,
  doFind,
  doAssignSolvedMeta,
  doFindSolvedMeta,
  doGetEnvLevel,
  doCollectArgs,
  returnAndPush,
  returnAndPushMetaView,
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
  | OpenNonLocal
  deriving (Eq, Show)

type EvalResult = Result EvalError

type EvalMonad = StateT Environment EvalResult

doPush :: Value -> EvalMonad ()
doPush val = modify (push val)

returnAndPush :: Value -> EvalMonad Value
returnAndPush val = doPush val >> return val

doPushMetaView :: VMetaView -> EvalMonad ()
doPushMetaView view = modify $ pushMetaView view

returnAndPushMetaView :: VMetaView -> EvalMonad Value
returnAndPushMetaView view = do
  (val, env) <- gets $ pushMetaView' view
  put env
  return val

doFind :: (CtxIndex id) => id -> EvalMonad (Maybe Value)
doFind idx = gets $ find idx

doAssignSolvedMeta :: (CtxIndex id) => id -> Value -> EvalMonad ()
doAssignSolvedMeta idx val = modify $ assignSolvedMeta idx val

doFindSolvedMeta :: (CtxIndex id) => id -> EvalMonad (Maybe Value)
doFindSolvedMeta idx = gets $ findSolvedMeta idx

doGetEnvLevel :: EvalMonad LevelId
doGetEnvLevel = gets envLevel

doCollectArgs :: LevelId -> EvalMonad [Value]
doCollectArgs = gets . collectArgs

returnNeutral :: Neutral -> EvalResult Value
returnNeutral = return . VNeutral

runEvalMonad :: EvalMonad a -> Environment -> EvalResult (a, Environment)
runEvalMonad = runStateT

evalEvalMonad :: EvalMonad a -> Environment -> EvalResult a
evalEvalMonad = evalStateT
