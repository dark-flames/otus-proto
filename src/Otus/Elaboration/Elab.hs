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
    cod <- doEvalCls codCls (vVar lvl)
    WFTerm bodyTm _ bodyVTy <- inherent ctx' body cod stage
    bodyTy <- doReadback (incrLvl lvl) bodyVTy
    return $ WFTerm (Lam bodyTm) stage (VPi dom (Closure (asEnv ctx) bodyTy))
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
  _ -> undefined
