module Otus.Normalize.Eval (
  evaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Foldable (foldlM)
import Otus.Ast
import Otus.Normalize.Env
import Otus.Normalize.Result
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
    return $ extendDynamic teleVal tyVal
  Ok subst res -> do
    substVal <- evalSubstitution env subst
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
    teleVal <- evalTelescope env tele
    tyVal <- go ty
    return $ extendLocal teleVal tyVal
  Guarded gSubst res -> do
    (gSubstVal, gEnv) <- runGuardedResult (evalGuardSubst gSubst) env
    resVal <- evaluate (intoEnv gEnv) res
    return $ VGuarded gSubstVal resVal
  -- Todo : Weakening
  -- Todo : LetOpen
  Error -> return VError
  _ -> undefined
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

evalConstraint :: Constraint -> GuardedEvalResult VConstraint
evalConstraint = undefined

evalGuardSeg :: GuardedSubstSeg -> GuardedEvalResult VGuardedSubstSeg
evalGuardSeg Unsolved = doPushVGSeg VUnsolved >> return VUnsolved
evalGuardSeg (Solved tm constrs) = do
  val <- doEvaluate tm
  constrVals <- mapM evalConstraint constrs
  return (VSolved val constrVals)

evalGuardSubst :: GuardedSubstitution -> GuardedEvalResult VGuardedSubstitution
evalGuardSubst (GSubst gSubst) = VGSubst <$> mapM evalGuardSeg gSubst

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

extendGuarded :: VGuardedSubstitution -> Value -> Value
extendGuarded vGuard = \case
  VGuarded innerVGuard val -> VGuarded (vGuard <> innerVGuard) val
  VError -> VError
  val -> VGuarded vGuard val

-- staging
evalForce :: Value -> Value
evalForce = \case
  VGuarded vGuarded val -> case forceVGuardedSubst vGuarded of
    Just vSubst -> VOk vSubst val
    Nothing -> VTyErr -- Should unsolved meta variables be allowed here?
  VError -> VTyErr
  VQuote val -> val
  val -> VForce val

evalLift :: Value -> Value
evalLift = \case
  VDynamic teleVal tyVal -> VLocal teleVal (evalLift tyVal)
  tyVal -> VLift tyVal

evalQuote :: Value -> Value
evalQuote = \case
  VOk substVal val -> extendGuarded (vSubstIntoGuarded substVal) (evalQuote val)
  VTyErr -> VError
  val -> VQuote val

vSubstIntoGuarded :: VSubstitution -> VGuardedSubstitution
vSubstIntoGuarded (VSubst vals) = VGSubst (map f vals)
  where
    f val = VSolved val []

forceVGuardedSeg :: VGuardedSubstSeg -> Maybe Value
forceVGuardedSeg = \case
  VUnsolved -> Nothing
  VSolved val constraints
    | null constraints -> Just val
    | otherwise -> Nothing

forceVGuardedSubst :: VGuardedSubstitution -> Maybe VSubstitution
forceVGuardedSubst (VGSubst segs) = VSubst <$> go segs
  where
    go = \case
      [] -> Just []
      seg : rest -> do
        val <- forceVGuardedSeg seg
        vals <- go rest
        return $ val : vals

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

doEvaluate :: Term -> GuardedEvalResult Value
doEvaluate tm = do
  env <- doGetEnv
  lift $ evaluate env tm
