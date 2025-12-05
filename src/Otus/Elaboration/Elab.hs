module Otus.Elaboration.Elab (
  WellFormedTerm (..),
  WellFormedTy (..),
  inherent,
  synthesis,
  inherentTy,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Elaboration.Context
import Otus.Elaboration.Control
import Otus.Elaboration.Expr
import Otus.Normalize

data WellFormedTerm = WFTerm
  { wfTerm :: ObjTerm,
    tmStage :: Stage,
    ty :: ObjValue
  }

data WellFormedTy = WFTy
  { wfTy :: ObjTerm,
    tyStage :: Stage,
    univLvl :: Universe
  }

inherentTy :: Context -> Expr -> ElabResult WellFormedTy
inherentTy = undefined

inherent :: Context -> Expr -> ObjValue -> Stage -> ElabResult WellFormedTerm
inherent ctx expr asTy stage = case (expr, asTy) of
  (ELam strId body, OVPi dom codCls) -> do
    (lvl, ctx') <- tryPushTy strId dom stage ctx
    cod <- doEvalCls (vVar lvl) codCls
    WFTerm bodyTm _ bodyVTy <- inherent ctx' body cod stage
    bodyTy <- doReadback (lvl + 1) bodyVTy
    return
      WFTerm
        { wfTerm = OLam bodyTm,
          tmStage = stage,
          ty = OVPi dom (ObjClosure (asEnv ctx) bodyTy)
        }
  _ -> undefined

synthesis :: Context -> Expr -> ElabResult WellFormedTerm
synthesis ctx = \case
  EVar stringId -> case findVTy stringId ctx of
    Just (idx, vty, stage) ->
      return
        WFTerm
          { wfTerm = OVar idx,
            tmStage = stage,
            ty = vty
          }
    Nothing -> throwError $ UnknownIdentifier stringId
  EApp fun arg -> do
    WFTerm funTm stage fTy <- synthesis ctx fun
    case fTy of
      OVPi dom codCls -> do
        WFTerm argTm stage' _ <- inherent ctx arg dom stage
        if stage' /= stage then
          throwError $ StageError arg stage
        else do
          vArg <- doEval argTm ctx
          vCod <- doEvalCls vArg codCls
          return
            WFTerm
              { wfTerm = OApp funTm argTm,
                tmStage = stage,
                ty = vCod
              }
      OVNeutral _ -> undefined
      _ -> undefined
  _ -> undefined
