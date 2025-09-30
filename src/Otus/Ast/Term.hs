module Otus.Ast.Term (
  InnerTerm(..), InnerTy,
  OuterTerm(..), OuterTy, Telescope(..)
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

data Telescope
  = TNil
  | TCons InnerTy Telescope
  deriving (Show, Eq)

type OuterTy= OuterTerm
data OuterTerm
    = OVar IndexId
    | OPi OuterTy OuterTy
    | OLam OuterTerm
    | OApp OuterTerm OuterTerm
    | OQuote Int InnerTerm
    | OLift Telescope InnerTy
    | OType Stage Universe 
    deriving (Show, Eq)
