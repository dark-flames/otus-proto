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
    Nothing -> throwError $ UnboundIndex idx
  Pi domain codomain -> do
    domainVal <- go domain
    let
      closure = Closure env codomain
    return $ VPi domainVal closure
  Lam body -> return $ VLam $ Closure env body
  App fn arg -> do
    fnVal <- go fn
    argVal <- go arg
    evalApp fnVal argVal
  Nat -> return VNat
  Zero -> return VZero
  Succ prev -> VSucc <$> go prev
  NatElim base step n -> do
    baseVal <- go base
    stepVal <- go step
    nVal <- go n
    evalNatElim baseVal stepVal nVal
  Id ty lhs rhs -> do
    tyVal <- go ty
    lhsVal <- go lhs
    rhsVal <- go rhs
    return $ VId tyVal lhsVal rhsVal
  Refl -> return VRefl
  J prop proof path -> do
    propVal <- go prop
    proofVal <- go proof
    pathVal <- go path
    evalJ propVal proofVal pathVal
  Type stage univ -> return $ VType stage univ
  Dynamic tele obj -> do
    teleVal <- evalTelescope env tele
    objVal <- go obj
    return $ VDynamic teleVal objVal
  Ok subst obj -> do
    substVal <- evalSubstitution env subst
    objVal <- go obj
    return $ VOk substVal objVal
  TyErr -> return VTyErr
  DBind next prev -> do
    nextVal <- go next
    prevVal <- go prev
    evalDbind nextVal prevVal
  _ -> throwError $ Anyhow "unimplemented"
  where
    go = evaluate env

-- evaluation of meta structures
evalClosure :: Closure -> Value -> EvalResult Value
evalClosure (Closure env tm) arg = evaluate (push env arg) tm

evalTelescope :: Environment -> Telescope -> EvalResult VTelescope
evalTelescope _ TNil = return $ VTele []
evalTelescope env (TCons tm tele) = do
  val <- evaluate env tm
  VTele teleVal <- evalTelescope (pushFreshVar env) tele
  return $ VTele (val : teleVal)

evalSubstitution :: Environment -> Substitution -> EvalResult VSubstitution
evalSubstitution _ SNil = return $ VSubst []
evalSubstitution env (SCons tm subst) = do
  val <- evaluate env tm
  VSubst substVal <- evalSubstitution env subst
  return $ VSubst (val : substVal)

-- evaluation of eliminations
evalApp :: Value -> Value -> EvalResult Value
evalApp fnVal argVal = case fnVal of
  VLam closure -> evalClosure closure argVal
  VNeutral neutral -> return $ VNeutral $ neutralApp neutral argVal
  _ -> throwError AppOnNonLambda

evalApp' :: Value -> [Value] -> EvalResult Value
evalApp' = foldlM evalApp

evalAppSubst :: Value -> VSubstitution -> EvalResult Value
evalAppSubst f (VSubst args) = go f args
  where
    go h = \case
      [] -> return h
      arg : rest -> do
        h' <- evalApp h arg
        go h' rest

evalNatElim :: Value -> Value -> Value -> EvalResult Value
evalNatElim baseVal stepVal = \case
  VZero -> return baseVal
  VSucc prevVal -> do
    recResVal <- evalNatElim baseVal stepVal prevVal
    evalApp' stepVal [prevVal, recResVal]
  VNeutral neutral -> return $ VNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError NatElimOnNonNat

evalJ :: Value -> Value -> Value -> EvalResult Value
evalJ prop proof = \case
  VRefl -> return proof
  VNeutral neutral -> return $ VNeutral $ NJ prop proof neutral
  _ -> throwError JOnNonId

evalDbind :: Value -> Value -> EvalResult Value
evalDbind next = \case
  VOk substVal prevVal -> do
    h <- evalAppSubst next substVal
    evalApp h prevVal
  VTyErr -> return VTyErr
  VNeutral neutral -> return $ VNeutral $ VDBind next neutral
  _ -> throwError DBindOnNonDynamic
