module Otus.Elaboration.Elab (
  synthesis,
  inherent,
) where

import Control.Monad.Error.Class

import Otus.Ast
import Otus.Elaboration.Context
import Otus.Elaboration.Control
import Otus.Elaboration.Judgement
import Otus.Normalize

synthesis :: Context -> PreSyntax -> ElaborationResult HybridTerm
synthesis ctx = \case
  SVar name -> case ctx @! name of
    Just (idx, MetaTy ty) -> return $ wfMetaValue (MVar idx) ty
    Just (idx, ObjTy ty) -> return $ wfObjTerm (Var idx) ty
    _ -> throwError $ UnboundName name
  SType l -> return $ wfObjTerm (Type l) (VType $ l + 1)
  _ -> undefined

inherent :: Context -> PreSyntax -> HybridType -> ElaborationResult HybridTerm
inherent = undefined
