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
import Data.Foldable (foldlM)

import qualified Data.Sequence as Seq

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
evalClosure arg (Closure env tm) = evaluate tm (push arg env)

evalClosure' :: ValueSeq -> Closure -> EvalResult Value
evalClosure' args (Closure env tm) = evaluate tm (push' args env)

evalClosureFresh :: Closure -> EvalResult (Value, Value)
evalClosureFresh (Closure env tm) = do
  let
    (arg, env') = pushFreshVar' env
  res <- evaluate tm env'
  return (res, arg)

evalClosureFreshN :: Int -> Closure -> EvalResult (Value, ValueSeq)
evalClosureFreshN n (Closure env tm) = do
  let
    (args, env') = pushFreshVarN' n env
  res <- evaluate tm env'
  return (res, args)

-- effect: push evaluated telesope to the environment
evalTelescope :: Telescope -> EvalMonad VTelescope
evalTelescope (Tele tys) = VTele <$> mapM go tys
  where
    go ty = do
      vty <- doEvaluate ty
      _ <- doPushFreshVar
      return vty

evalSubstitution :: Substitution -> Environment -> EvalResult VSubstitution
evalSubstitution (Subst tms) env = VSubst <$> mapM (`evaluate` env) tms

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
    constrVals <- lift $ mapM (`evalConstraint` env) constrs
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
  VSig <$> mapM (evalMetaDef env) defs

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
          Just args -> VOk (VSubst args) <$> evalClosure' args cls
          Nothing -> return VTyErr -- unsolved meta variable
      Nothing -> return VTyErr -- conflict
  VError -> return VTyErr
  VNeutral neu -> return $ VNeutral $ NForce neu
  _ -> throwError ForceOnNonLocal

forceVSignature :: VSignature -> EvalResult (Maybe ValueSeq)
forceVSignature (VSig defs) = foldlM f (Just Seq.Empty) defs
  where
    f :: Maybe ValueSeq -> VMetaDefinition -> EvalResult (Maybe ValueSeq)
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

evalApp' :: Value -> ValueSeq -> EvalResult Value
evalApp' = foldlM evalApp

evalNatElim :: Value -> Value -> Value -> EvalResult Value
evalNatElim baseVal stepVal = \case
  VZero _ -> return baseVal
  VSucc prevVal -> do
    recResVal <- evalNatElim baseVal stepVal prevVal
    evalApp' stepVal $ Seq.fromList [prevVal, recResVal]
  VNeutral neutral -> returnNeutral $ NNatElim baseVal stepVal neutral
  _ -> throwError NatElimOnNonNat

evalDbind :: Value -> Closure -> EvalResult Value
evalDbind val nextCls = case val of
  VOk (VSubst subst) prevVal -> do
    let
      subst' = subst |> prevVal
    res <- evalClosure' subst' nextCls
    return $ VOk (VSubst subst') res
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NDBind neutral nextCls
  _ -> throwError DBindOnNonDynamic

evalAssign :: Int -> VSignature -> Value -> EvalResult Value
evalAssign n vsig = \case
  VGuarded m vsig' inner ->
    assert (n + size vsig' == m) $ -- by typing
      return $
        VGuarded n (vsig <> vsig') inner
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NAssign n vsig neutral
  _ -> throwError AssignOnNonLocal

evalOpen :: Value -> Value -> EvalResult Value
evalOpen vPrev vNext = case vPrev of
  VGuarded n (VSig defs) prevInnerCls ->
    let
      vSig = VSig $ defs |> VMSolved prevInnerCls
    in
      evalAssign n vSig vNext
  VTyErr -> return VTyErr
  VNeutral neutral -> returnNeutral $ NOpen neutral vNext
  _ -> throwError OpenNonLocal

-- utils
doEvaluate :: Term -> EvalMonad Value
doEvaluate tm = do
  env <- get
  lift $ evaluate tm env
