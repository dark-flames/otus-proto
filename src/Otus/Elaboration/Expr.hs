module Otus.Elaboration.Expr (
  Expr (..),
) where

import Otus.Common

data Expr
  = EVar String
  | EPi String Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | EType Universe
  | EDyn
  | EHole
  deriving (Eq, Show)
