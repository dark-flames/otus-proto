module Otus.Normalize.Object.Error (
  ObjEvalError (..),
  ObjEvalResult,
  ObjEvalResultT,
  ObjEvalMonad,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Object.Value

data ObjEvalError
  = Anyhow String
  | UnboundIndex IndexId
  | UnknownMeta LevelId
  | AppOnNonLambda
  | NatElimOnNonNat
  | JOnNonId
  | DBindOnNonDynamic
  | ForceOnNonLocal
  | AssignOnNonLocal
  | OpenNonLocal
  | UnsolvableTmEq VTelescope ObjValue ObjValue ObjValue
  deriving (Eq, Show)

type ObjEvalResult = Result ObjEvalError

type ObjEvalResultT = ResultT ObjEvalError

type ObjEvalMonad = EvalMonad ObjEvalError ObjValue
