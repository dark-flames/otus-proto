module Otus.TypeCheck.Error (
  TypeError (..),
  TypeCheckResult,
  mapEvalResult,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize

data TypeError
  = EvalError EvalError
  | CannotInferIndex
  | CannotInferObjAsMeta
  | CannotCheckAsMetaType MetaTerm MetaValue
  | CannotCheckAsThunk MetaTerm MetaValue
  | CannotBindOn MetaTerm MetaValue
  | ComputationUnify MetaTerm MetaValue MetaValue
  | ValueUnify MetaTerm MetaValue MetaValue
  | CannotInferValue MetaTerm
  | CannotInferComputation MetaTerm
  | ComputationEffErr MetaTerm EffectSet EffectSet
  | ExpectedToBeComputationTy MetaTerm
  | ExpectedToBeValueTy MetaTerm
  | ExpectedToBeMetaPi MetaTerm
  | AnyhowTyErr String
  deriving (Eq, Show)

type TypeCheckResult = Result TypeError

mapEvalResult :: EvalResult a -> TypeCheckResult a
mapEvalResult = \case
  Success a -> Success a
  Failure e -> Failure (EvalError e)
