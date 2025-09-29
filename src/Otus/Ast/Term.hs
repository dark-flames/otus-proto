module Otus.Ast.Term (
  InnerTerm(..), InnerTy,
) where

import           Otus.Ast.Id
import           Otus.Ast.Univ

type InnerTy= InnerTerm
data InnerTerm
    = IVar IndexId
    | IPi InnerTy InnerTy
    | ILam InnerTerm
    | IApp InnerTerm InnerTerm
    | INat
    | IZero
    | ISuc InnerTerm
    | INatElim InnerTerm InnerTerm InnerTerm InnerTerm
    | IType Universe
    deriving (Show, Eq)
