module Otus.Normalize.Object.Eval (
  evaluateObj,
  evalClosure,
  evalClosure',
  evalClosureFresh,
  evalClosureFreshN,
  evalApp,
  evalConstraint,
  evalSignature,
  evalSubstitution,
  doEvaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Strict (get, lift)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Object.Value

evaluateObj :: ObjTerm -> ObjEnv -> EvalResult ObjValue
evaluateObj tm env = case tm of
  OVar idx -> case env @? idx of
    Just val -> return val
    Nothing -> throwError $ UnboundIndex idx
  OPi domain codomain -> do
    domainVal <- go domain
    let closure = Closure env codomain
    return $ OVPi domainVal closure
  OLam body -> return $ OVLam $ Closure env body
  OApp fn arg -> do
    fnVal <- go fn
    argVal <- go arg
    evalApp fnVal argVal
  OType stage univ -> return $ OVType stage univ
  where
    go tm' = evaluateObj tm' env

-- evaluation of meta structures
evalClosure :: ObjValue -> Closure -> EvalResult ObjValue
evalClosure arg (Closure env tm) = evaluateObj tm (env |> arg)

evalClosure' :: (Item l ~ ObjValue, Sequence l) => l -> Closure -> EvalResult ObjValue
evalClosure' args (Closure env tm) = evaluateObj tm (env >< args)

evalClosureFresh :: Closure -> EvalResult (ObjValue, ObjValue)
evalClosureFresh (Closure env tm) = do
  let (arg, env') = pushFreshVar' env
  res <- evaluateObj tm env'
  return (res, arg)

evalClosureFreshN :: Int -> Closure -> EvalResult (ObjValue, ObjValueSeq)
evalClosureFreshN n (Closure env tm) = do
  let (args, env') = pushFreshVarN' n env
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

evalSubstitution :: Substitution -> ObjEnv -> EvalResult VSubstitution
evalSubstitution (Subst tms) env = seqMapM (`evaluateObj` env) tms

evalConstraint :: Constraint -> ObjEnv -> EvalResult VConstraint
evalConstraint constr env = case constr of
  TyEq tele lhs rhs -> do
    (vTele, env') <- runEvalMonad (evalTelescope tele) env
    vLhs <- evaluateObj lhs env'
    vRhs <- evaluateObj rhs env'
    return $ VTyEq vTele vLhs vRhs
  TmEq tele lhs rhs ty -> do
    (vTele, env') <- runEvalMonad (evalTelescope tele) env
    vLhs <- evaluateObj lhs env'
    vRhs <- evaluateObj rhs env'
    vTy <- evaluateObj ty env'
    return $ VTmEq vTele vLhs vRhs vTy

-- effect: push evaluated meta def to the environment
evalMetaDef :: ObjEnv -> MetaDefinition -> ObjEvalMonad VMetaDefinition
evalMetaDef baseEnv def = case def of
  Unsolved -> doPushFreshVar >> return VMUnsolved
  Guarded tm constrs -> do
    env <- get
    constrVals <- lift $ seqMapM (`evalConstraint` env) constrs
    _ <- doPushFreshVar
    return (VMGuarded (Closure baseEnv tm) constrVals)
  MSolved tm -> do
    val <- doEvaluate tm
    doPush val
    return $ VMSolved $ Closure baseEnv tm

-- effect: push evaluated signature to the environment
evalSignature :: Signature -> ObjEvalMonad VSignature
evalSignature (Sig defs) = do
  env <- get
  seqMapM (evalMetaDef env) defs

-- staging

-- evaluation of eliminations
evalApp :: ObjValue -> ObjValue -> EvalResult ObjValue
evalApp fnVal argVal = case fnVal of
  OVLam closure -> evalClosure argVal closure
  OVNeutral neutral -> returnNeutral $ neutralApp neutral argVal
  _ -> throwError AppOnNonLambda

-- utils
doEvaluate :: ObjTerm -> ObjEvalMonad ObjValue
doEvaluate tm = do
  env <- get
  lift $ evaluateObj tm env
