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

import Control.Monad.State.Lazy (MonadState (put), StateT (runStateT), evalStateT, gets, modify)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env

type EvalMonad err env = StateT env (Result err)

doPush :: (Environment env) => Element env -> EvalMonad err env ()
doPush val = modify (push val)

returnAndPush :: (Environment env) => Element env -> EvalMonad err env (Element env)
returnAndPush val = doPush val >> return val

doPushFreshVar :: (Environment env) => EvalMonad err env (Element env)
doPushFreshVar = do
  (val, env) <- gets pushFreshVar
  put env
  return val

doFind :: (Environment env, SeqIndex id) => id -> EvalMonad err env (Maybe (Element env))
doFind idx = gets $ find idx

doGetEnvLevel :: (Environment env) => EvalMonad err env LevelId
doGetEnvLevel = gets envLevel

returnNeutral :: (Monad m, Value val) => Neutral val -> m val
returnNeutral = return . fromNeutral

runEvalMonad :: EvalMonad err env a -> env -> Result err (a, env)
runEvalMonad = runStateT

evalEvalMonad :: EvalMonad err env a -> env -> Result err a
evalEvalMonad = evalStateT
