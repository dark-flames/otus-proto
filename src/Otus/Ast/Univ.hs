module Otus.Ast.Univ (
  Universe (..),
  Stage (..),
) where

data Universe
  = UZero
  | USucc Universe
  deriving (Eq, Show)

data Stage
  = Meta
  | Object
  deriving (Eq, Show)
