module Otus.Normalize.Eval (
  evaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (foldlM)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Value

evaluate :: Term -> Environment -> EvalResult Value
evaluate tm env = case tm of
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
    teleVal <- evalTelescope tele env
    tyVal <- go ty
    return $ extendDynamic teleVal tyVal
  Ok subst res -> do
    substVal <- evalSubstitution subst env
    resVal <- go res
    return $ extendOk substVal resVal
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
    teleVal <- evalTelescope tele env
    tyVal <- go ty
    return $ extendLocal teleVal tyVal
  Guarded sig res -> do
    (env', vSig) <- evalSignature sig env
    resVal <- evaluate res env'
    let
      guardedVal = extendGuarded vSig resVal
    -- todo : solve
    return guardedVal
  -- Todo : Weakening
  -- Todo : LetOpen
  Error -> return VError
  _ -> undefined
  where
    go tm' = evaluate tm' env

-- evaluation of meta structures
evalClosure :: Closure -> Value -> EvalResult Value
evalClosure (Closure env tm) arg = evaluate tm (push arg env)

evalClosure' :: Closure -> [Value] -> EvalResult Value
evalClosure' (Closure env tm) args = evaluate tm (push' args env)

evalTelescope :: Telescope -> Environment -> EvalResult VTelescope
evalTelescope (Tele tys) env = VTele . snd <$> lensIterM evaluate (const pushFreshVar) tys env

evalSubstitution :: Substitution -> Environment -> EvalResult VSubstitution
evalSubstitution (Subst tms) env = VSubst <$> mapM (`evaluate` env) tms

evalConstraint :: Constraint -> Environment -> EvalResult VConstraint
evalConstraint = undefined

evalMetaDef :: MetaDefinition -> Environment -> EvalResult VMetaDefinition
evalMetaDef def env = case def of
  Unsolved -> return VUnsolved
  (Solved tm constrs) -> do
    val <- evaluate tm env
    constrVals <- mapM (`evalConstraint` env) constrs
    return (VSolved val constrVals)

evalSignature :: Signature -> Environment -> EvalResult (Environment, VSignature)
evalSignature (Sig sig) env = mapSnd VSig <$> lensIterM evalMetaDef pushMetaDef sig env

-- currying of telescope
extendDynamic :: VTelescope -> Value -> Value
extendDynamic vTele = \case
  VDynamic vTele' val -> VDynamic (vTele <> vTele') val
  val -> VDynamic vTele val

extendOk :: VSubstitution -> Value -> Value
extendOk vSubst = \case
  VOk vSubst' val -> VOk (vSubst <> vSubst') val
  VTyErr -> VTyErr
  val -> VOk vSubst val

extendLocal :: VTelescope -> Value -> Value
extendLocal vTele = \case
  VLocal vTele' val -> VLocal (vTele <> vTele') val
  val -> VLocal vTele val

extendGuarded :: VSignature -> Value -> Value
extendGuarded vSig = \case
  VGuarded innerVSig val -> VGuarded (vSig <> innerVSig) val
  VError -> VError
  val -> VGuarded vSig val

-- staging
evalForce :: Value -> Value
evalForce = \case
  VGuarded vSig val -> case forceVSignature vSig of
    Just vSubst -> VOk vSubst val
    Nothing -> VTyErr -- Should unsolved meta variables be allowed here?
  VError -> VTyErr
  VQuote val -> val
  val -> VForce val

evalLift :: Value -> Value
evalLift = \case
  VDynamic vTele vTy -> VLocal vTele (evalLift vTy)
  vTy -> VLift vTy

evalQuote :: Value -> Value
evalQuote = \case
  VOk vSubst val -> extendGuarded (quoteVSubst vSubst) (evalQuote val)
  VTyErr -> VError
  val -> VQuote val

quoteVSubst :: VSubstitution -> VSignature
quoteVSubst (VSubst vals) = VSig (map f vals)
  where
    f val = VSolved val []

forceVMetaDef :: VMetaDefinition -> Maybe Value
forceVMetaDef = \case
  VUnsolved -> Nothing
  VSolved val constraints
    | null constraints -> Just val
    | otherwise -> Nothing

forceVSignature :: VSignature -> Maybe VSubstitution
forceVSignature (VSig defs) = VSubst <$> mapM forceVMetaDef defs

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
    return $ extendOk (VSubst subst') res
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ VDBind nextCls neutral
  _ -> throwError DBindOnNonDynamic

-- utils
returnNeutral :: Neutral -> EvalResult Value
returnNeutral = return . VNeutral
