module Otus.Ast.Univ (
  Universe(..)
) where

data Universe
  = UZero
  | USucc Universe
  deriving (Show, Eq)
