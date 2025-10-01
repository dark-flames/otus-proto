module Otus.Ast.Univ (
  Universe(..),
  Stage(..)
) where

data Universe
  = UZero
  | USucc Universe
  deriving (Show, Eq)

data Stage
  = Outer
  | Inner
  deriving (Show, Eq)