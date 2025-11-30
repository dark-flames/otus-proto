module Otus.Elaboration.Expr (
  Expr (..),
) where

import Otus.Common

data Expr
  = EVar String
  | EPi String Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | ENat
  | EZero
  | ESucc
  | ENatElim Expr Expr Expr
  | EType Universe
  | EDyn
  deriving (Eq, Show)
