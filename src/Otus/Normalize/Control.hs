module Otus.Normalize.Control (
  EvalError (..),
  EvalResult,
  EvalResultT,
  EvalMonad,
  ObjEvalMonad,
  MetaEvalMonad,
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
import Otus.Normalize.Meta.Value
import Otus.Normalize.Object.Value

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
  | UnsolvableTmEq VTelescope ObjValue ObjValue ObjValue
  deriving (Eq, Show)

type EvalResult = Result EvalError

type EvalResultT = ResultT EvalError

type EvalMonad val = StateT (Environment val) EvalResult

type ObjEvalMonad = EvalMonad ObjValue

type MetaEvalMonad = EvalMonad MetaValue

doPush :: val -> EvalMonad val ()
doPush val = modify (|> val)

returnAndPush :: val -> EvalMonad val val
returnAndPush val = doPush val >> return val

doPushFreshVar :: (Value val) => EvalMonad val val
doPushFreshVar = do
  (val, env) <- gets pushFreshVar'
  put env
  return val

doFind :: (SeqIndex id) => id -> EvalMonad val (Maybe val)
doFind idx = gets $ find idx

doGetEnvLevel :: EvalMonad val LevelId
doGetEnvLevel = gets envLevel

returnNeutral :: Neutral -> EvalResult ObjValue
returnNeutral = return . OVNeutral

runEvalMonad :: EvalMonad val a -> Environment val -> EvalResult (a, Environment val)
runEvalMonad = runStateT

evalEvalMonad :: EvalMonad val a -> Environment val -> EvalResult a
evalEvalMonad = evalStateT
