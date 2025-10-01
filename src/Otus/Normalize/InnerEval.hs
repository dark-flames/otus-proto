module Otus.Normalize.Inner.Eval(
  evalInner
) where

import          Data.List.NonEmpty (singleton, (<|))
import           Otus.Ast
import           Otus.Normalize.Err
import           Control.Monad.Error.Class

evalInnerCls :: InnerClosure -> InnerVal -> NormalizeResult InnerVal
evalInnerCls (Closure env body) argVal = evalInner (pushInner env argVal) body

evalInnerCls' :: InnerClosure -> [InnerVal] -> NormalizeResult InnerVal
evalInnerCls' (Closure env body) argVals = evalInner (pushInner' env argVals) body

-- Elimination
evalIApp :: InnerVal -> InnerVal -> NormalizeResult InnerVal
evalIApp fnVal argVal = case fnVal of
  IVLam cls -> evalInnerCls cls argVal
  INeutral neu -> return $ INeutral $ case neu of
    INApp headNeu args -> INApp headNeu (argVal <| args)
    _ -> INApp neu (singleton argVal)
  _ -> throwError $ InnerAppOnNoneLambda fnVal

evalINatElim :: InnerClosure -> InnerClosure -> InnerVal -> NormalizeResult InnerVal
evalINatElim base step scrutinee = case scrutinee of
  IVZero -> evalInnerCls base IVZero
  IVSuc n -> do
    recRes <- evalINatElim base step n
    evalInnerCls' step [n, recRes]
  INeutral neu -> return $ INeutral $ INNatElim neu base step
  _ -> throwError $ InnerNatElimOnNonNat scrutinee

-- Evaluate Inner Term
evalInner :: Env -> InnerTerm -> NormalizeResult InnerVal
evalInner env tm = case tm of
  IVar idx -> case find env idx of
    Just (InnerEl v)  -> return v
    Just (OuterEl _)  -> throwError $ OuterVarInInner idx
    Nothing -> throwError $ UnboundIndex Inner idx
  IPi domain codomain -> do
    domainVal <- evalInner env domain
    let codomainCls = Closure env codomain
    return $ IVPi domainVal codomainCls
  ILam body -> return $ IVLam (Closure env body)
  IApp fn arg -> do
    fnVal <- evalInner env fn
    argVal <- evalInner env arg
    evalIApp fnVal argVal
  INat -> return IVNat
  IZero -> return IVZero
  ISuc n -> IVSuc <$> evalInner env n
  INatElim base step scrutinee -> do
    let baseCls = Closure env base
    let stepCls = Closure env step
    scrutineeVal <- evalInner env scrutinee
    evalINatElim baseCls stepCls scrutineeVal
  IType u -> return $ IVType u


