module Otus.Ast.Value (
  InnerVal(..), InnerTyVal
) where


import           Data.List.NonEmpty
import           Otus.Ast.Id

data EnvSeg
  = InnerEl InnerVal
  | OuterEl
  deriving (Show, Eq)

newtype Env = Env [EnvSeg]
  deriving (Show, Eq)

data Closure val = Closure {
    closureEnv  :: Env,
    closureBody :: val
  } deriving (Show, Eq)

type InnerClosure = Closure InnerVal
data InnerNeutral
  = INVar LevelId
  | INApp InnerVal (NonEmpty InnerVal)
  | INNatElim InnerVal InnerVal InnerVal InnerNeutral
  deriving (Show, Eq)

type InnerTyVal = InnerVal
data InnerVal
  = InnerNeutral InnerNeutral
  | InnerVPi InnerVal InnerClosure
  | InnerVLam InnerClosure
  | InnerVNat
  | InnerVZero
  | InnerVSuc InnerVal
  | InnerVType Int
  deriving (Show, Eq)
