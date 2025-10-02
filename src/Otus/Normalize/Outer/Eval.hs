module Otus.Normalize.Outer.Eval
  ( evalOCls,
    evalOCls',
    evalOApp,
    evalOuter,
  )
where

import Control.Monad.Error.Class
import Data.List.NonEmpty (singleton, (<|))
import Otus.Ast
import Otus.Normalize.Err

evalOCls :: OuterClosure -> OuterVal -> NormalizeResult OuterVal
evalOCls (Closure env body) argVal = evalOuter (pushOuter env argVal) body

evalOCls' :: OuterClosure -> [OuterVal] -> NormalizeResult OuterVal
evalOCls' (Closure env body) argVals = evalOuter (pushOuter' env argVals) body

-- Elimination
evalOApp :: OuterVal -> OuterVal -> NormalizeResult OuterVal
evalOApp fnVal argVal = case fnVal of
  OVLam cls -> evalOCls cls argVal
  ONeutral neu -> return $ ONeutral $ case neu of
    ONApp headNeu args -> ONApp headNeu (argVal <| args)
    _ -> ONApp neu (singleton argVal)
  _ -> throwError $ OuterAppOnNoneLambda fnVal

evalOuter :: Env -> OuterTerm -> NormalizeResult OuterVal
evalOuter env tm = case tm of
  OVar idx -> case find env idx of
    Just (OuterEl v) -> return v
    Just (InnerEl _) -> throwError $ InnerVarInOuter idx
    Nothing -> throwError $ UnboundIndex Inner idx
  OPi domain codomain -> do
    domainVal <- evalOuter env domain
    let codomainCls = Closure env codomain
    return $ OVPi domainVal codomainCls
  OLam body -> return $ OVLam (Closure env body)
  OApp fn arg -> do
    fnVal <- evalOuter env fn
    argVal <- evalOuter env arg
    evalOApp fnVal argVal
  OType stage univ -> return $ OVType stage univ
  _ -> undefined
