module Otus.Normalize.Eval (
  evaluateObj,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (foldlM)
import Otus.Ast
import Otus.Normalize.Env
import Otus.Normalize.Err
import Otus.Normalize.Value

evaluateObj :: Environment -> Term -> EvalResult Value
evaluateObj env tm = case tm of
  Var idx -> case find env idx of
    Just val -> return val
    Nothing -> throwError $ UnboundIndex Object idx
  Pi domain codomain -> do
    domainVal <- evaluateObj env domain
    let closure = Closure env codomain
    return $ VPi domainVal closure
  Lam body -> return $ VLam $ Closure env body
  App fn arg -> do
    fnVal <- evaluateObj env fn
    argVal <- evaluateObj env arg
    evalObjApp fnVal argVal
  Nat -> return VNat
  Zero -> return VZero
  Succ prev -> VSucc <$> evaluateObj env prev
  NatElim base step n -> do
    baseVal <- evaluateObj env base
    stepVal <- evaluateObj env step
    nVal <- evaluateObj env n
    evalObjNatElim baseVal stepVal nVal
  Id ty lhs rhs -> do
    tyVal <- evaluateObj env ty
    lhsVal <- evaluateObj env lhs
    rhsVal <- evaluateObj env rhs
    return $ VId tyVal lhsVal rhsVal
  Refl -> return VRefl
  J prop proof path -> do
    propVal <- evaluateObj env prop
    proofVal <- evaluateObj env proof
    pathVal <- evaluateObj env path
    evalObjJ propVal proofVal pathVal
  Type stage univ -> return $ VType stage univ
  Dynamic tele obj -> do
    teleVal <- evalTelescope env tele
    objVal <- evaluateObj env obj
    return $ VDynamic teleVal objVal
  Ok subst obj -> do
    substVal <- evalSubstitution env subst
    objVal <- evaluateObj env obj
    return $ VOk substVal objVal
  TyErr -> return VTyErr
  DBind next prev -> do
    nextVal <- evaluateObj env next
    prevVal <- evaluateObj env prev
    evalObjDbind nextVal prevVal
  _ -> throwError $ Anyhow "unimplemented"

-- evaluation of meta structures
evalObjClosure :: Closure -> Value -> EvalResult Value
evalObjClosure (Closure env tm) arg = evaluateObj (push env arg) tm

evalTelescope :: Environment -> Telescope -> EvalResult VTelescope
evalTelescope _ TNil = return VTNil
evalTelescope env (TCons tm tele) = do
  val <- evaluateObj env tm
  teleVal <- evalTelescope (freshVar env) tele
  return $ VTCons val teleVal

evalSubstitution :: Environment -> Substitution -> EvalResult VSubstitution
evalSubstitution _ SNil = return VSNil
evalSubstitution env (SCons tm subst) = do
  val <- evaluateObj env tm
  substVal <- evalSubstitution env subst
  return $ VSCons val substVal

-- evaluation of eliminations
evalObjApp :: Value -> Value -> EvalResult Value
evalObjApp fnVal argVal = case fnVal of
  VLam closure -> evalObjClosure closure argVal
  VNeutral neutral -> return $ VNeutral $ neutralApp neutral argVal
  _ -> throwError $ AppOnNonLambda Object

evalObjApp' :: Value -> [Value] -> EvalResult Value
evalObjApp' = foldlM evalObjApp

evalObjAppSubst :: Value -> VSubstitution -> EvalResult Value
evalObjAppSubst h VSNil = return h
evalObjAppSubst h (VSCons arg subst) = do
  h' <- evalObjApp h arg
  evalObjAppSubst h' subst

evalObjNatElim :: Value -> Value -> Value -> EvalResult Value
evalObjNatElim baseVal stepVal = \case
  VZero -> return baseVal
  VSucc prevVal -> do
    recResVal <- evalObjNatElim baseVal stepVal prevVal
    evalObjApp' stepVal [prevVal, recResVal]
  VNeutral neutral -> return $ VNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError $ NatElimOnNonNat Object

evalObjJ :: Value -> Value -> Value -> EvalResult Value
evalObjJ prop proof = \case
  VRefl -> return proof
  VNeutral neutral -> return $ VNeutral $ NJ prop proof neutral
  _ -> throwError $ JOnNonId Object

evalObjDbind :: Value -> Value -> EvalResult Value
evalObjDbind next = \case
  VOk substVal prevVal -> do
    h <- evalObjAppSubst next substVal
    evalObjApp h prevVal
  VTyErr -> return VTyErr
  VNeutral neutral -> return $ VNeutral $ VDBind next neutral
  _ -> throwError DBindOnNonDynamic
