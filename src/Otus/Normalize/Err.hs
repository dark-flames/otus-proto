module Otus.Normalize.Err(
  NormalizeErr(..),
  NormalizeResult
) where
import           Otus.Ast
import           Otus.Common

data NormalizeErr
  = Anyhow String
  | UnboundIndex Stage IndexId
  | OuterVarInInner IndexId
  | InnerAppOnNoneLambda InnerVal
  | InnerNatElimOnNonNat InnerVal
  deriving (Show, Eq)

type NormalizeResult = Result NormalizeErr
