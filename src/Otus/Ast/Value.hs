module Otus.Ast.Value (
  Env(..), EnvSeg(..), Closure(..),
  InnerVal(..), InnerTyVal, InnerNeutral(..), InnerClosure,
  OuterVal(..), OuterTyVal, OuterNeutral(..), OuterClosure, VTelescope(..), TeleClosure
) where


import           Data.List.NonEmpty
import           Otus.Ast.Id
import           Otus.Ast.Term
import           Otus.Ast.Univ

-- Environment

data EnvSeg
  = InnerEl InnerVal
  | OuterEl OuterVal
  deriving (Show, Eq)

newtype Env = Env [EnvSeg]
  deriving (Show, Eq)

data Closure val = Closure {
    closureEnv  :: Env,
    closureBody :: val
  } deriving (Show, Eq)

type InnerClosure = Closure InnerTerm
data InnerNeutral
  = INVar LevelId
  | INApp InnerVal (NonEmpty InnerVal)
  | INNatElim InnerVal InnerVal InnerVal InnerNeutral
  deriving (Show, Eq)

type InnerTyVal = InnerVal
data InnerVal
  = INeutral InnerNeutral
  | IVPi InnerTyVal InnerClosure
  | IVLam InnerClosure
  | IVNat
  | IVZero
  | IVSuc InnerVal
  | IVType Universe
  deriving (Show, Eq)

-- Outer Value

type OuterClosure = Closure OuterVal
type TeleClosure = Closure Telescope

data VTelescope
  = TVNil
  | TVCons InnerTyVal TeleClosure
  deriving (Show, Eq)

data OuterNeutral
  = ONVar LevelId
  | ONApp OuterVal (NonEmpty InnerVal)
  deriving (Show, Eq)

type OuterTyVal = OuterVal
data OuterVal
  = ONeutral OuterNeutral
  | OVPi OuterTyVal OuterClosure
  | OVLam OuterClosure
  | OVQuote Int InnerVal
  | OVLift [InnerTyVal] InnerVal
  | OVType Stage Universe
  deriving (Show, Eq)
