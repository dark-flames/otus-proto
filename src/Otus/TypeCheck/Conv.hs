module Otus.TypeCheck.Conv (
  valueConversionCheck,
  computationConversionCheck,
) where

import Otus.Ast
import Otus.Normalize
import Otus.TypeCheck.Error

valueConversionCheck :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult bool
valueConversionCheck = undefined

computationConversionCheck :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult bool
computationConversionCheck = undefined
