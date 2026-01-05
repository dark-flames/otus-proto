module Otus.Normalize.Meta.Eval (
  evaluateMeta,
  evalMetaType,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env (Environment (eempty, pushFreshVarN'))
import Otus.Normalize.Meta.Error
import Otus.Normalize.Meta.Value
import Otus.Normalize.Object

buildObjEnv :: Int -> ObjEnv
buildObjEnv s = pushFreshVarN' s eempty

evaluateMeta :: MetaTerm -> MetaEnv -> MetaEvalResult MetaValue
evaluateMeta tm env = case tm of
  MVar idx -> case env @? idx of
    Just val -> return val
    Nothing -> throwError $ MetaUnboundIndex idx
  MLam body -> return $ MVLam $ MetaClosure env body
  MApp fn arg -> do
    vFn <- go fn
    vArg <- go arg
    evalMetaApp vFn vArg
  MGuarded domainSize problem subst -> do
    let objEnv = buildObjEnv domainSize
    vProblem <- fromObjResult $ evalProblem problem objEnv
    return $ MVConsistent domainSize vProblem subst
  MErr -> return MVErr
  where
    go tm' = evaluateMeta tm' env

evalMetaType :: MetaType -> MetaEvalResult MetaVType
evalMetaType = \case
  MFn dom cod -> do
    vDom <- evalMetaType dom
    vCod <- evalMetaType cod
    return $ MVFn vDom vCod
  MInner tele -> MVInner <$> evalObjEvalMonad (evalTelescope tele)

evalMetaClosure :: MetaClosure -> MetaValue -> MetaEvalResult MetaValue
evalMetaClosure (MetaClosure env tm) arg = evaluateMeta tm (env |> arg)

evalMetaApp :: MetaValue -> MetaValue -> MetaEvalResult MetaValue
evalMetaApp vFn vArg = case vFn of
  MVLam cls -> evalMetaClosure cls vArg
  MVNeutral neu -> returnNeutral $ metaNeutralApp neu vArg
  _ -> throwError MetaAppOnNonFn
