module Otus.Normalize.Meta.Eval (
  evaluateMeta,
  evalMetaType,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Meta.Error
import Otus.Normalize.Meta.Value
import Otus.Normalize.Object

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
  MGuarded domainSize problem record -> do
    let mctx = buildMetaCtx domainSize
    let objEnv = ObjEnv mctx empty
    res <- fromObjResult $ evalProblem problem objEnv >>= solveProblem mctx
    case res of
      Just (vProb, mctx') -> do
        vRecord <- fromObjResult $ evalSubstitution record (ObjEnv mctx' empty)
        return $ MVConsistent mctx' vProb vRecord undefined
      Nothing -> return MVErr
  MErr -> return MVErr
  MProduct lhs rhs -> do
    vLhs <- go lhs
    vRhs <- go rhs
    evalMetaProduct vLhs vRhs
  MCSubst _prev _problem _record _tm -> do
    _ <- evalMetaCSubst undefined undefined undefined
    undefined
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

evalMetaProduct :: MetaValue -> MetaValue -> MetaEvalResult MetaValue
evalMetaProduct lhs rhs = case (lhs, rhs) of
  (MVErr, _) | isInner rhs -> return MVErr
  (_, MVErr) | isInner lhs -> return MVErr
  (MVConsistent _lCtx _lProb _lRecord _, MVConsistent _rCtx _rProb _rRecord _) -> undefined -- todo: renaming
  (MVNeutral neu, _) -> returnNeutral $ MNProductL neu rhs
  (_, MVNeutral neu) -> returnNeutral $ MNProductR lhs neu
  _ -> throwError MetaCombainOnNonInner

evalMetaCSubst :: MetaValue -> Problem -> Record -> MetaEvalResult MetaValue
evalMetaCSubst prev problem record = case prev of
  MVErr -> return MVErr
  (MVConsistent _mctx _prevProblem _prevVRecord _) -> undefined
  MVNeutral neu -> returnNeutral $ MNSubst neu problem record
  _ -> throwError MetaCSubstOnNonInner
