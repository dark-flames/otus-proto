module Otus.Normalize.Eval (
  evaluate,
  evalClosure,
  evalClosure',
  doEvaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Strict (get, lift)
import Data.Foldable (foldlM, foldrM)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Solve
import Otus.Normalize.Value

evaluate :: Term -> Environment -> EvalResult Value
evaluate tm env = case tm of
  Var idx -> case find idx env of
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
  Type stage univ -> return $ VType stage univ
  -- Object
  Force metaTm -> go metaTm >>= evalForce
  Dynamic tele ty -> do
    (teleVal, _) <- evalTelescope tele env
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
  Local tele ty -> do
    (teleVal, env') <- evalTelescope tele env
    tyVal <- evaluate ty env'
    return $ VLocal teleVal tyVal
  Guarded sig inner -> do
    vSig <- evalEvalMonad (evalSignature sig) env
    res <- solveSignature (envLevel env) vSig
    case res of
      Just vSig' -> return $ VGuarded vSig' (Closure env inner)
      _ -> return VError -- conflict
      -- Todo : Weakening
      -- Todo : LetOpen
  Error -> return VError
  _ -> undefined
  where
    go tm' = evaluate tm' env

-- evaluation of meta structures
evalClosure :: Value -> Closure -> EvalResult Value
evalClosure arg (Closure env tm) = evaluate tm (push arg env)

evalClosure' :: [Value] -> Closure -> EvalResult Value
evalClosure' args (Closure env tm) = evaluate tm (push' args env)

evalTelescope :: Telescope -> Environment -> EvalResult (VTelescope, Environment)
evalTelescope (Tele tys) env = mapFst VTele <$> lensIterM evaluate (const pushFreshVar) tys env

evalSubstitution :: Substitution -> Environment -> EvalResult VSubstitution
evalSubstitution (Subst tms) env = VSubst <$> mapM (`evaluate` env) tms

evalConstraint :: Constraint -> Environment -> EvalResult VConstraint
evalConstraint constr env = case constr of
  TyEq tele lhs rhs -> uncurry3 VTyEq <$> evalTuple tele lhs rhs
  TmEq tele lhs rhs -> uncurry3 VTmEq <$> evalTuple tele lhs rhs
  where
    evalTuple tele lhs rhs = do
      (vTele, env') <- evalTelescope tele env
      vLhs <- evaluate lhs env'
      vRhs <- evaluate rhs env'
      return (vTele, vLhs, vRhs)

evalMetaDef :: Environment -> MetaDefinition -> EvalMonad VMetaDefinition
evalMetaDef baseEnv def = case def of
  MUnsolved -> doPushFreshVar >> return VMUnsolved
  MGuarded tm constrs -> do
    env <- get
    constrVals <- lift $ mapM (`evalConstraint` env) constrs
    _ <- doPushFreshVar
    return (VMGuarded (Closure baseEnv tm) constrVals)
  MSolved tm -> do
    val <- doEvaluate tm
    doPush val
    return $ VMSolved $ Closure baseEnv tm

evalSignature :: Signature -> EvalMonad VSignature
evalSignature (Sig defs) = do
  env <- get
  VSig <$> mapM (evalMetaDef env) defs

-- currying of telescope
extendDynamic :: VTelescope -> Value -> Value
extendDynamic vTele = \case
  VDynamic vTele' val -> extendDynamic (vTele <> vTele') val
  val -> VDynamic vTele val

extendOk :: VSubstitution -> Value -> Value
extendOk vSubst = \case
  VOk vSubst' val -> extendOk (vSubst <> vSubst') val
  VTyErr -> VTyErr
  val -> VOk vSubst val

-- staging
evalForce :: Value -> EvalResult Value
evalForce = \case
  VGuarded vSig cls ->
    forceVSignature vSig >>= \case
      Just args -> VOk (VSubst args) <$> evalClosure' args cls
      Nothing -> return VTyErr
  VError -> return VTyErr
  VNeutral neu -> return $ VNeutral $ NForce neu
  _ -> throwError ForceOnNonLocal

forceVSignature :: VSignature -> EvalResult (Maybe [Value])
forceVSignature (VSig defs) = foldrM f (return []) defs
  where
    f :: VMetaDefinition -> Maybe [Value] -> EvalResult (Maybe [Value])
    f vDef = \case
      Just args -> case vDef of
        VMSolved cls -> do
          val <- evalClosure' args cls
          return $ Just (args ++ [val])
        _ -> return Nothing
      Nothing -> return Nothing

-- evaluation of eliminations
evalApp :: Value -> Value -> EvalResult Value
evalApp fnVal argVal = case fnVal of
  VLam closure -> evalClosure argVal closure
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

evalDbind :: Closure -> Value -> EvalResult Value
evalDbind nextCls = \case
  VOk (VSubst subst) prevVal -> do
    let
      subst' = subst ++ [prevVal]
    res <- evalClosure' subst' nextCls
    return $ extendOk (VSubst subst') res
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NDBind nextCls neutral
  _ -> throwError DBindOnNonDynamic

-- utils
doEvaluate :: Term -> EvalMonad Value
doEvaluate tm = do
  env <- get
  lift $ evaluate tm env
