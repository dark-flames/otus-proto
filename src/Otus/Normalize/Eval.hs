module Otus.Normalize.Eval (
  evaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (foldlM)
import Otus.Ast
import Otus.Normalize.Env
import Otus.Normalize.Err
import Otus.Normalize.Value

evaluate :: Environment -> Term -> EvalResult Value
evaluate env tm = case tm of
  Var idx -> case find env idx of
    Just val -> return val
    Nothing -> throwError $ UnboundIndex Object idx
  Pi domain codomain -> do
    domainVal <- evaluate' domain
    let
      closure = Closure env codomain
    return $ VPi domainVal closure
  Lam body -> return $ VLam $ Closure env body
  App fn arg -> do
    fnVal <- evaluate' fn
    argVal <- evaluate' arg
    evalApp evalStage fnVal argVal
  Nat -> return VNat
  Zero -> return VZero
  Succ prev -> VSucc <$> evaluate' prev
  NatElim base step n -> do
    baseVal <- evaluate' base
    stepVal <- evaluate' step
    nVal <- evaluate' n
    evalNatElim evalStage baseVal stepVal nVal
  Id ty lhs rhs -> do
    tyVal <- evaluate' ty
    lhsVal <- evaluate' lhs
    rhsVal <- evaluate' rhs
    return $ VId tyVal lhsVal rhsVal
  Refl -> return VRefl
  J prop proof path -> do
    propVal <- evaluate' prop
    proofVal <- evaluate' proof
    pathVal <- evaluate' path
    evalJ evalStage propVal proofVal pathVal
  Type stage univ -> return $ VType stage univ
  Dynamic tele obj -> do
    teleVal <- evalTelescope env tele
    objVal <- evaluate' obj
    return $ VDynamic teleVal objVal
  Ok subst obj -> do
    substVal <- evalSubstitution env subst
    objVal <- evaluate' obj
    return $ VOk substVal objVal
  TyErr -> return VTyErr
  DBind next prev -> do
    nextVal <- evaluate' next
    prevVal <- evaluate' prev
    evalDbind nextVal prevVal
  _ -> throwError $ Anyhow "unimplemented"
  where
    evalStage = envStage env
    evaluate' = evaluate env

-- evaluation of meta structures
evalClosure :: Closure -> Value -> EvalResult Value
evalClosure (Closure env tm) arg = evaluate (push env arg) tm

evalTelescope :: Environment -> Telescope -> EvalResult VTelescope
evalTelescope _ TNil = return VTNil
evalTelescope env (TCons tm tele) = do
  val <- evaluate env tm
  teleVal <- evalTelescope (freshVar env) tele
  return $ VTCons val teleVal

evalSubstitution :: Environment -> Substitution -> EvalResult VSubstitution
evalSubstitution _ SNil = return VSNil
evalSubstitution env (SCons tm subst) = do
  val <- evaluate env tm
  substVal <- evalSubstitution env subst
  return $ VSCons val substVal

-- evaluation of eliminations
evalApp :: Stage -> Value -> Value -> EvalResult Value
evalApp stage fnVal argVal = case fnVal of
  VLam closure -> evalClosure closure argVal
  VNeutral neutral -> return $ VNeutral $ neutralApp neutral argVal
  _ -> throwError $ AppOnNonLambda stage

evalApp' :: Stage -> Value -> [Value] -> EvalResult Value
evalApp' stage = foldlM $ evalApp stage

evalAppSubst :: Stage -> Value -> VSubstitution -> EvalResult Value
evalAppSubst _ h VSNil = return h
evalAppSubst stage h (VSCons arg subst) = do
  h' <- evalApp stage h arg
  evalAppSubst stage h' subst

evalNatElim :: Stage -> Value -> Value -> Value -> EvalResult Value
evalNatElim stage baseVal stepVal = \case
  VZero -> return baseVal
  VSucc prevVal -> do
    recResVal <- evalNatElim stage baseVal stepVal prevVal
    evalApp' stage stepVal [prevVal, recResVal]
  VNeutral neutral -> return $ VNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError $ NatElimOnNonNat stage

evalJ :: Stage -> Value -> Value -> Value -> EvalResult Value
evalJ stage prop proof = \case
  VRefl -> return proof
  VNeutral neutral -> return $ VNeutral $ NJ prop proof neutral
  _ -> throwError $ JOnNonId stage

evalDbind :: Value -> Value -> EvalResult Value
evalDbind next = \case
  VOk substVal prevVal -> do
    h <- evalAppSubst Object next substVal
    evalApp Object h prevVal
  VTyErr -> return VTyErr
  VNeutral neutral -> return $ VNeutral $ VDBind next neutral
  _ -> throwError DBindOnNonDynamic
