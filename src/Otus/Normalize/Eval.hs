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
  Succ pre -> VSucc <$> evaluateObj env pre
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
  _ -> throwError $ Anyhow "unimplemented"

evalObjClosure :: Closure -> Value -> EvalResult Value
evalObjClosure (Closure env tm) arg = evaluateObj (push env arg) tm

evalObjApp :: Value -> Value -> EvalResult Value
evalObjApp fnVal argVal = case fnVal of
  VLam closure -> evalObjClosure closure argVal
  VNeutral neutral -> return $ VNeutral $ neutralApp neutral argVal
  _ -> throwError $ AppOnNonLambda Object

evalObjApp' :: Value -> [Value] -> EvalResult Value
evalObjApp' = foldlM evalObjApp

evalObjNatElim :: Value -> Value -> Value -> EvalResult Value
evalObjNatElim baseVal stepVal = \case
  VZero -> return baseVal
  VSucc pre -> do
    recResVal <- evalObjNatElim baseVal stepVal pre
    evalObjApp' stepVal [pre, recResVal]
  VNeutral neutral -> return $ VNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError $ NatElimOnNonNat Object

evalObjJ :: Value -> Value -> Value -> EvalResult Value
evalObjJ prop proof = \case
  VRefl -> return proof
  VNeutral neutral -> return $ VNeutral $ NJ prop proof neutral
  _ -> throwError $ JOnNonId Object
