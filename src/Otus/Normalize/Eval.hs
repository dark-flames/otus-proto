module Otus.Normalize.Eval (
  evaluate,
  evalClosure,
  evalClosure',
  evalClosureFresh,
  evalClosureFreshN,
  evalApp,
) where

import Control.Exception (assert)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Strict (get, lift)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Solve
import Otus.Normalize.Value

evaluate :: Term -> Environment -> EvalResult Value
evaluate tm env = case tm of
  Var idx -> case env @? idx of
    Just val -> return val
    Nothing -> throwError $ UnboundIndex idx
  Pi domain codomain -> do
    domainVal <- go domain
    let closure = Closure env codomain
    return $ VPi domainVal closure
  Lam body -> return $ VLam $ Closure env body
  App fn arg -> do
    fnVal <- go fn
    argVal <- go arg
    evalApp fnVal argVal
  Type stage univ -> return $ VType stage univ
  -- Object
  Force metaTm -> go metaTm >>= evalForce (envLevel env)
  Dynamic tele ty -> do
    (teleVal, env') <- runEvalMonad (evalTelescope tele) env
    tyVal <- evaluate ty env'
    return $ VDynamic teleVal tyVal
  Ok subst res -> do
    substVal <- evalSubstitution subst env
    resVal <- go res
    return $ VOk substVal resVal
  TyErr -> return VTyErr
  DBind prev next -> do
    prevVal <- go prev
    evalDbind prevVal (Closure env next)
  -- Meta
  Local freeTele boundTele ty -> do
    ((freeVTele, boundVTele), env') <-
      runEvalMonad
        ( do
            f <- evalTelescope freeTele
            b <- evalTelescope boundTele
            return (f, b)
        )
        env
    tyVal <- evaluate ty env'
    return $ VLocal freeVTele boundVTele tyVal
  Guarded n sig inner -> do
    vSig <- evalEvalMonad (evalSignature sig) (pushFreshVarN n env)
    return $ VGuarded n vSig (Closure env inner)
  -- Todo : Weakening
  -- Todo : LetOpen
  Assign n sig local -> do
    vSig <- evalEvalMonad (evalSignature sig) (pushFreshVarN n env)
    vLocal <- go local
    evalAssign n vSig vLocal
  Open prev next -> do
    vPre <- go prev
    vNext <- go next
    evalOpen vPre vNext
  Error -> return VError
  where
    go tm' = evaluate tm' env

-- evaluation of meta structures
evalClosure :: Value -> Closure -> EvalResult Value
evalClosure arg (Closure env tm) = evaluate tm (env |> arg)

evalClosure' :: (Item l ~ Value, Sequence l) => l -> Closure -> EvalResult Value
evalClosure' args (Closure env tm) = evaluate tm (env >< args)

evalClosureFresh :: Closure -> EvalResult (Value, Value)
evalClosureFresh (Closure env tm) = do
  let (arg, env') = pushFreshVar' env
  res <- evaluate tm env'
  return (res, arg)

evalClosureFreshN :: Int -> Closure -> EvalResult (Value, ValueSeq)
evalClosureFreshN n (Closure env tm) = do
  let (args, env') = pushFreshVarN' n env
  res <- evaluate tm env'
  return (res, args)

-- effect: push evaluated telesope to the environment
evalTelescope :: Telescope -> EvalMonad VTelescope
evalTelescope (Tele tys) = seqMapM go tys
  where
    go ty = do
      vty <- doEvaluate ty
      _ <- doPushFreshVar
      return vty

evalSubstitution :: Substitution -> Environment -> EvalResult VSubstitution
evalSubstitution (Subst tms) env = seqMapM (`evaluate` env) tms

evalConstraint :: Constraint -> Environment -> EvalResult VConstraint
evalConstraint constr env = case constr of
  TyEq tele lhs rhs -> do
    (vTele, env') <- runEvalMonad (evalTelescope tele) env
    vLhs <- evaluate lhs env'
    vRhs <- evaluate rhs env'
    return $ VTyEq vTele vLhs vRhs
  TmEq tele lhs rhs ty -> do
    (vTele, env') <- runEvalMonad (evalTelescope tele) env
    vLhs <- evaluate lhs env'
    vRhs <- evaluate rhs env'
    vTy <- evaluate ty env'
    return $ VTmEq vTele vLhs vRhs vTy

-- effect: push evaluated meta def to the environment
evalMetaDef :: Environment -> MetaDefinition -> EvalMonad VMetaDefinition
evalMetaDef baseEnv def = case def of
  MUnsolved -> doPushFreshVar >> return VMUnsolved
  MGuarded tm constrs -> do
    env <- get
    constrVals <- lift $ seqMapM (`evalConstraint` env) constrs
    _ <- doPushFreshVar
    return (VMGuarded (Closure baseEnv tm) constrVals)
  MSolved tm -> do
    val <- doEvaluate tm
    doPush val
    return $ VMSolved $ Closure baseEnv tm

-- effect: push evaluated signature to the environment
evalSignature :: Signature -> EvalMonad VSignature
evalSignature (Sig defs) = do
  env <- get
  seqMapM (evalMetaDef env) defs

-- staging
evalForce :: LevelId -> Value -> EvalResult Value
evalForce base = \case
  -- _n == 0 was guarded by type system
  VGuarded _n vSig cls ->
    solveSignature base vSig >>= \case
      -- try solve vSig
      Just vSig' ->
        forceVSignature vSig' >>= \case
          -- ensure all meta is Solved
          Just vSubst -> VOk vSubst <$> evalClosure' vSubst cls
          Nothing -> returnNeutral $ NForceUnsolved vSig' cls -- unsolved meta variable
      Nothing -> return VTyErr -- conflict
  VError -> return VTyErr
  VNeutral neu -> returnNeutral $ NForce neu
  _ -> throwError ForceOnNonLocal

forceVSignature :: VSignature -> EvalResult (Maybe VSubstitution)
forceVSignature = seqFoldlM f (Just empty)
  where
    f :: Maybe VSubstitution -> VMetaDefinition -> EvalResult (Maybe VSubstitution)
    f res vDef = case res of
      Just args -> case vDef of
        VMSolved cls -> do
          val <- evalClosure' args cls
          return $ Just (args |> val)
        _ -> return Nothing
      Nothing -> return Nothing

-- evaluation of eliminations
evalApp :: Value -> Value -> EvalResult Value
evalApp fnVal argVal = case fnVal of
  VLam closure -> evalClosure argVal closure
  VNeutral neutral -> returnNeutral $ neutralApp neutral argVal
  _ -> throwError AppOnNonLambda

evalApp' :: (Item l ~ Value, Sequence l) => Value -> l -> EvalResult Value
evalApp' = seqFoldlM evalApp

evalDbind :: Value -> Closure -> EvalResult Value
evalDbind val nextCls = case val of
  VOk vSubst prevVal -> do
    let vSubst' = vSubst |> prevVal
    res <- evalClosure' vSubst' nextCls
    return $ VOk vSubst res
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NDBind neutral nextCls
  _ -> throwError DBindOnNonDynamic

evalAssign :: Int -> VSignature -> Value -> EvalResult Value
evalAssign n vsig = \case
  VGuarded m vsig' inner ->
    assert (n + size vsig' == m) $ -- by typing
      return $
        VGuarded n (vsig >< vsig') inner
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NAssign n vsig neutral
  _ -> throwError AssignOnNonLocal

evalOpen :: Value -> Value -> EvalResult Value
evalOpen vPrev vNext = case vPrev of
  VGuarded n vSig prevInnerCls ->
    let vSig' = vSig |> VMSolved prevInnerCls
    in evalAssign n vSig' vNext
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NOpen neutral vNext
  _ -> throwError OpenNonLocal

-- utils
doEvaluate :: Term -> EvalMonad Value
doEvaluate tm = do
  env <- get
  lift $ evaluate tm env
