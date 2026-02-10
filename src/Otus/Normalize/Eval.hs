module Otus.Normalize.Eval (
  evaluateMeta,
  evaluate,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Value

-- meta

evaluateMeta :: MetaTerm -> Environment -> EvalResult MetaValue
evaluateMeta = undefined

-- object
evaluateObjClosure :: Value -> ObjClosure -> EvalResult Value
evaluateObjClosure p cls = evaluate (clsTm cls) (clsEnv cls ||> p)

evaluateTelescope :: Telescope -> Environment -> EvalResult (VTelescope, Environment)
evaluateTelescope tele env = foldlM go (mempty, env) tele
  where
    go :: (VTelescope, Environment) -> Term -> EvalResult (VTelescope, Environment)
    go (prev, e) tm = do
      val <- evaluate tm e
      return (prev |> val, freshVar' e)

evaluateRecord :: Record -> Environment -> EvalResult VRecord
evaluateRecord record env = mapM (`evaluate` env) record

evaluateApp :: Value -> Value -> EvalResult Value
evaluateApp vFn vParam = case vFn of
  VLam cls -> evaluateObjClosure vParam cls
  Neutral h spine -> return $ Neutral h (SApp spine vParam)
  _ -> throwError AppOnNonLambda

evaluateFirst :: Value -> EvalResult Value
evaluateFirst = \case
  VList (val :<| _) -> return val
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SFirst spine)
  _ -> throwError ProjOnNonRecord

evaluateRest :: Value -> EvalResult Value
evaluateRest = \case
  VList (_ :<| rest) -> return $ VList rest
  VList _ -> throwError ProjOnEmptyRecord
  Neutral h spine -> return $ Neutral h (SRest spine)
  _ -> throwError ProjOnNonRecord

evaluateSplicing :: MetaValue -> Environment -> EvalResult Value
evaluateSplicing val env = case val of
  MVQuote record -> VList <$> evaluateRecord record env
  MNeutral var spine -> return $ Neutral (NSplicing var spine) SNil
  _ -> throwError SplicingNonMeta

evaluate :: Term -> Environment -> EvalResult Value
evaluate tm env = case tm of
  Var idx -> case env @? idx of
    Nothing -> throwError $ UnboundIndex idx
    Just (MetaVal _) -> throwError $ InvalidMetaVar idx
    Just (ObjVal val) -> return val
  Pi dom cod -> do
    vDom <- evaluate dom env
    let cls = Closure env cod
    return $ VPi vDom cls
  Lam body -> return $ VLam (Closure env body)
  App fn param -> do
    vFn <- evaluate fn env
    vParam <- evaluate param env
    evaluateApp vFn vParam
  Record tele -> VRecord . fst <$> evaluateTelescope tele env
  List record -> VList <$> evaluateRecord record env
  First l -> evaluate l env >>= evaluateFirst
  Rest l -> evaluate l env >>= evaluateRest
  Splicing meta -> do
    vMeta <- evaluateMeta meta env
    evaluateSplicing vMeta env
  _ -> throwError $ Anyhow "unimplement"
