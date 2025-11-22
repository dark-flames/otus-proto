module Otus.Normalize.Control (
  EvalError (..),
  EvalResult,
  SolveMonad,
  doAssignMeta,
  doLookupMeta,
  doUpdateMeta,
) where

import Control.Monad.State.Strict (StateT, gets, modify)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env
import Otus.Normalize.Value

data EvalError
  = Anyhow String
  | UnboundIndex IndexId
  | AppOnNonLambda
  | NatElimOnNonNat
  | JOnNonId
  | DBindOnNonDynamic
  | OpenNonLocal
  deriving (Eq, Show)

type EvalResult = Result EvalError

type SolveMonad = StateT MetaEnv EvalResult

doAssignMeta :: LevelId -> Value -> SolveMonad ()
doAssignMeta lvl val = modify $ assignMeta lvl val

doLookupMeta :: LevelId -> SolveMonad (Maybe Value)
doLookupMeta lvl = gets $ lookupMeta lvl

doUpdateMeta :: LevelId -> VMetaDefinition -> SolveMonad ()
doUpdateMeta lvl def = modify $ updateMeta lvl def
