module Otus.Normalize.Value (
  Closure (..),
  Value (..),
  Neutral (..),
  neutralApp,
) where

import Data.List.NonEmpty
import Otus.Ast
import Otus.Normalize.Env

data Closure = Closure Environment Term
  deriving (Eq, Show)

data Neutral
  = NVar LevelId
  | NApp Neutral (NonEmpty Value)
  | NNatElim Value Value Neutral
  | NJ Value Value Neutral
  deriving (Eq, Show)

data Value
  = VNeutral Neutral
  | VPi Value Closure
  | VLam Closure
  | VNat
  | VZero
  | VSucc Value
  | VId Value Value Value
  | VRefl
  deriving (Eq, Show)

neutralApp :: Neutral -> Value -> Neutral
neutralApp n arg = case n of
  NApp h args -> NApp h (arg <| args)
  _ -> NApp n $ singleton arg
