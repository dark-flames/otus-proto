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
  Nat s -> return $ VNat s
  Zero s -> return $ VZero s
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
  -- Object
  Force metaTm -> do
    metaVal <- go metaTm
    return $ evalForce metaVal
  Dynamic tele ty -> do
    teleVal <- evalTelescope env tele
    tyVal <- go ty
    return $ evalDynamic teleVal tyVal
  Ok subst res -> do
    substVal <- evalSubstitution env subst
    resVal <- go res
    return $ evalOk substVal resVal
  TyErr -> return VTyErr
  DBind next prev -> do
    prevVal <- go prev
    evalDbind (Closure env next) prevVal
  -- Meta
  Lift objTy -> do
    objTyVal <- go objTy
    return $ evalLift objTyVal
  Quote objTm -> do
    objVal <- go objTm
    return $ evalQuote objVal
  Local tele ty -> do
    teleVal <- evalTelescope env tele
    tyVal <- go ty
    return $ evalLocal teleVal tyVal
  Partial domain subst res -> do
    domainVal <- evalTelescope env domain
    let
      env' = pushMetaTele env domainVal
    substVal <- evalSubstitution env' subst
    resVal <- evaluate env' res
    return $ evalPartial domainVal substVal resVal
  Error -> return VError
  LetOpen next prev -> do
    prevVal <- go prev
    evalOpen (Closure env next) prevVal
  Unify _ _ -> throwError $ Anyhow "unimplemented"
  where
    go = evaluate env

-- evaluation of meta structures
evalClosure :: Closure -> Value -> EvalResult Value
evalClosure (Closure env tm) arg = evaluate (push env arg) tm

evalClosure' :: Closure -> [Value] -> EvalResult Value
evalClosure' (Closure env tm) args = evaluate (push' env args) tm

evalTelescope :: Environment -> Telescope -> EvalResult VTelescope
evalTelescope env (Tele tys) = VTele <$> go env tys
  where
    go e = \case
      [] -> return []
      ty : tele -> do
        vTy <- evaluate e ty
        vTele <- go (pushFreshVar e) tele
        return (vTy : vTele)

evalSubstitution :: Environment -> Substitution -> EvalResult VSubstitution
evalSubstitution env (Subst tms) = VSubst <$> mapM (evaluate env) tms

-- currying of telescope
evalDynamic :: VTelescope -> Value -> Value
evalDynamic vTele = \case
  VDynamic vTele' val -> VDynamic (vTele <> vTele') val
  val -> VDynamic vTele val

evalOk :: VSubstitution -> Value -> Value
evalOk vSubst = \case
  VOk vSubst' val -> VOk (vSubst <> vSubst') val
  VTyErr -> VTyErr
  val -> VOk vSubst val

evalLocal :: VTelescope -> Value -> Value
evalLocal vTele = \case
  VLocal vTele' val -> VLocal (vTele <> vTele') val
  val -> VLocal vTele val

evalPartial :: VTelescope -> VSubstitution -> Value -> Value
evalPartial vTele vSubst = \case
  VPartial vTele' vSubst' val ->
    let
      resTele = vTele <> vTele'
      resSubst = vSubst <> vSubst'
    in
      VPartial resTele resSubst val
  VError -> VError
  val -> VPartial vTele vSubst val

-- staging
evalForce :: Value -> Value
evalForce = \case
  VPartial (VTele domain) vSubst res -> case domain of
    [] -> evalOk vSubst (evalForce res)
    _ -> VError
  VQuote val -> val
  val -> VForce val

evalLift :: Value -> Value
evalLift = \case
  VDynamic teleVal tyVal -> VLocal teleVal (evalLift tyVal)
  tyVal -> VLift tyVal

evalQuote :: Value -> Value
evalQuote = \case
  VOk substVal val -> evalPartial (VTele []) substVal (evalQuote val)
  VError -> VError
  val -> VQuote val

-- evaluation of eliminations
evalApp :: Value -> Value -> EvalResult Value
evalApp fnVal argVal = case fnVal of
  VLam closure -> evalClosure closure argVal
  VNeutral neutral -> returnNeutral $ neutralApp neutral argVal
  _ -> throwError AppOnNonLambda

evalApp' :: Value -> [Value] -> EvalResult Value
evalApp' = foldlM evalApp

evalNatElim :: Value -> Value -> Value -> EvalResult Value
evalNatElim baseVal stepVal = \case
  VZero _ -> return baseVal
  VSucc prevVal -> do
    recResVal <- evalNatElim baseVal stepVal prevVal
    evalApp' stepVal [prevVal, recResVal]
  VNeutral neutral -> returnNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError NatElimOnNonNat

evalJ :: Value -> Value -> Value -> EvalResult Value
evalJ prop proof = \case
  VRefl -> return proof
  VNeutral neutral -> returnNeutral $ NJ prop proof neutral
  _ -> throwError JOnNonId

evalDbind :: Closure -> Value -> EvalResult Value
evalDbind nextCls = \case
  VOk (VSubst subst) prevVal -> do
    let
      subst' = subst ++ [prevVal]
    res <- evalClosure' nextCls subst'
    return $ evalOk (VSubst subst') res
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ VDBind nextCls neutral
  _ -> throwError DBindOnNonDynamic

evalOpen :: Closure -> Value -> EvalResult Value
evalOpen nextCls = \case
  VPartial teleVal (VSubst substVals) val -> do
    let
      subst' = substVals ++ [val]
    res <- evalClosure' nextCls subst'
    return $ evalPartial teleVal (VSubst subst') res
  VNeutral neutral -> returnNeutral $ VOpen nextCls neutral
  _ -> throwError OpenNonLocal

-- utils
returnNeutral :: Neutral -> EvalResult Value
returnNeutral = return . VNeutral
