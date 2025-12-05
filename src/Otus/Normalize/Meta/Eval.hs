module Otus.Normalize.Meta.Eval (
  evaluateMeta,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Meta.Error
import Otus.Normalize.Meta.Value

evaluateMeta :: MetaTerm -> MetaEnv -> MetaEvalResult MetaValue
evaluateMeta tm env = case tm of
  MVar idx -> case env @? idx of
    Just val -> return val
    Nothing -> throwError $ MetaUnboundIndex idx
  _ -> undefined
