module Otus.Normalize.Object.Eval (
  evaluateObj,
  evalClosure,
  evalClosure',
  evalClosureFresh,
  evalClosureFreshN,
  evalApp,
  evalApp',
  evalTelescope,
  evalConstraint,
  evalProblem,
  evalSubstitution,
  doEvaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Lazy (get, lift)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Object.Error
import Otus.Normalize.Object.Value

evaluateObj :: ObjTerm -> ObjEnv -> ObjEvalResult ObjValue
evaluateObj tm env = case tm of
  OVar idx -> case find idx env of
    Just val -> return val
    Nothing -> throwError $ ObjUnboundIndex idx
  OMeta mid -> case findMeta mid env of
    Just (Solved val) -> return val
    Just UnSolved -> return $ OVMeta mid
    Nothing -> throwError $ ObjUnknownMeta mid
  OPi domain codomain -> do
    domainVal <- go domain
    let closure = ObjClosure env codomain
    return $ OVPi domainVal closure
  OLam body -> return $ OVLam $ ObjClosure env body
  OApp fn arg -> do
    fnVal <- go fn
    argVal <- go arg
    evalApp fnVal argVal
  OType -> return OVType
  where
    go tm' = evaluateObj tm' env

-- evaluation of meta structures
evalClosure :: ObjValue -> ObjClosure -> ObjEvalResult ObjValue
evalClosure arg (ObjClosure env tm) = evaluateObj tm (push arg env)

evalClosure' :: (Item l ~ ObjValue, Sequence l) => l -> ObjClosure -> ObjEvalResult ObjValue
evalClosure' args (ObjClosure env tm) = evaluateObj tm (pushN args env)

evalClosureFresh :: ObjClosure -> ObjEvalResult (ObjValue, ObjValue)
evalClosureFresh (ObjClosure env tm) = do
  let (arg, env') = pushFreshVar env
  res <- evaluateObj tm env'
  return (res, arg)

evalClosureFreshN :: Int -> ObjClosure -> ObjEvalResult (ObjValue, ObjValueSeq)
evalClosureFreshN n (ObjClosure env tm) = do
  let (args, env') = pushFreshVarN n env
  res <- evaluateObj tm env'
  return (res, args)

-- effect: push evaluated telesope to the environment
evalTelescope :: Telescope -> ObjEvalMonad VTelescope
evalTelescope (Tele tys) = seqMapM go tys
  where
    go ty = do
      vty <- doEvaluate ty
      _ <- doPushFreshVar
      return vty

evalSubstitution :: Record -> ObjEnv -> ObjEvalResult VRecord
evalSubstitution (Record tms) env = seqMapM (`evaluateObj` env) tms

evalConstraint :: Constraint -> ObjEnv -> ObjEvalResult VConstraint
evalConstraint constr env = case constr of
  TmEq ctxSize lhs rhs -> do
    let env' = pushFreshVarN' ctxSize env
    vLhs <- evaluateObj lhs env'
    vRhs <- evaluateObj rhs env'
    return $ VTmEq ctxSize vLhs vRhs

evalProblem :: Problem -> ObjEnv -> ObjEvalResult VProblem
evalProblem (Sig defs) env = seqMapM (`evalConstraint` env) defs

-- evaluation of eliminations
evalApp :: ObjValue -> ObjValue -> ObjEvalResult ObjValue
evalApp fnVal argVal = case fnVal of
  OVLam closure -> evalClosure argVal closure
  OVNeutral neutral -> returnNeutral $ objNeutralApp neutral argVal
  _ -> throwError ObjAppOnNonLambda

evalApp' :: (Item l ~ ObjValue, Sequence l) => ObjValue -> l -> ObjEvalResult ObjValue
evalApp' = seqFoldlM evalApp

-- utils
doEvaluate :: ObjTerm -> ObjEvalMonad ObjValue
doEvaluate tm = do
  env <- get
  lift $ evaluateObj tm env
