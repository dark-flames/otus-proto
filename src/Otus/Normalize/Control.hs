module Otus.Normalize.Control (
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

type EvalMonad err val = StateT (Environment val) (Result err)

doPush :: val -> EvalMonad err val ()
doPush val = modify (|> val)

returnAndPush :: val -> EvalMonad err val val
returnAndPush val = doPush val >> return val

doPushFreshVar :: (Value val) => EvalMonad err val val
doPushFreshVar = do
  (val, env) <- gets pushFreshVar'
  put env
  return val

doFind :: (SeqIndex id) => id -> EvalMonad err val (Maybe val)
doFind idx = gets $ find idx

doGetEnvLevel :: EvalMonad val err LevelId
doGetEnvLevel = gets envLevel

returnNeutral :: (Value val) => Neutral val -> Result err val
returnNeutral = return . fromNeutral

runEvalMonad :: EvalMonad err val a -> Environment val -> Result err (a, Environment val)
runEvalMonad = runStateT

evalEvalMonad :: EvalMonad err val a -> Environment val -> Result err a
evalEvalMonad = evalStateT
