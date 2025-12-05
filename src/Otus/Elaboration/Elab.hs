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
import Otus.Normalize.Value

data WellFormedTerm = WFTerm
  { wfTerm :: Term,
    tmStage :: Stage,
    ty :: Value
  }

data WellFormedTy = WFTy
  { wfTy :: Term,
    tyStage :: Stage,
    univLvl :: Universe
  }

inherentTy :: Context -> Expr -> ElabResult WellFormedTy
inherentTy = undefined

inherent :: Context -> Expr -> Value -> Stage -> ElabResult WellFormedTerm
inherent ctx expr asTy stage = case (expr, asTy) of
  (ELam strId body, VPi dom codCls) -> do
    (lvl, ctx') <- tryPushTy strId dom stage ctx
    cod <- doEvalCls (vVar lvl) codCls
    WFTerm bodyTm _ bodyVTy <- inherent ctx' body cod stage
    bodyTy <- doReadback (lvl + 1) bodyVTy
    return
      WFTerm
        { wfTerm = Lam bodyTm,
          tmStage = stage,
          ty = VPi dom (Closure (asEnv ctx) bodyTy)
        }
  _ -> undefined

synthesis :: Context -> Expr -> ElabResult WellFormedTerm
synthesis ctx = \case
  EVar stringId -> case findVTy stringId ctx of
    Just (idx, vty, stage) ->
      return
        WFTerm
          { wfTerm = Var idx,
            tmStage = stage,
            ty = vty
          }
    Nothing -> throwError $ UnknownIdentifier stringId
  EApp fun arg -> do
    WFTerm funTm stage fTy <- synthesis ctx fun
    case fTy of
      VPi dom codCls -> do
        WFTerm argTm stage' _ <- inherent ctx arg dom stage
        if stage' /= stage then
          throwError $ StageError arg stage
        else do
          vArg <- doEval argTm ctx
          vCod <- doEvalCls vArg codCls
          return
            WFTerm
              { wfTerm = App funTm argTm,
                tmStage = stage,
                ty = vCod
              }
      VNeutral _ -> undefined
      _ -> undefined
  _ -> undefined
