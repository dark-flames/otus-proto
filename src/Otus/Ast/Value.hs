module Otus.Ast.Value
  ( Env (..),
    EnvSeg (..),
    Closure (..),
    pushInner,
    pushOuter,
    pushInner',
    pushOuter',
    InnerVal (..),
    InnerTyVal,
    InnerNeutral (..),
    InnerClosure,
    OuterVal (..),
    OuterTyVal,
    OuterNeutral (..),
    OuterClosure,
    VTelescope (..),
    TeleClosure,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Otus.Ast.Id
import Otus.Ast.Term
import Otus.Ast.Univ

-- Environment
data EnvSeg
  = InnerEl InnerVal
  | OuterEl OuterVal
  deriving (Show, Eq)

newtype Env = Env [EnvSeg]
  deriving (Show, Eq)

data Closure val = Closure
  { closureEnv :: Env,
    closureBody :: val
  }
  deriving (Show, Eq)

instance Contextual Env where
  ctxLength (Env segs) = length segs

instance CtxLike Env EnvSeg where
  (Env segs) !? i
    | i < 0 || i >= length segs = Nothing
    | otherwise = Just $ segs !! i
  push (Env segs) seg = Env $ seg : segs

pushInner :: Env -> InnerVal -> Env
pushInner env = push env . InnerEl

pushInner' :: Env -> [InnerVal] -> Env
pushInner' env vs = foldl pushInner env (reverse vs)

pushOuter :: Env -> OuterVal -> Env
pushOuter env = push env . OuterEl

pushOuter' :: Env -> [OuterVal] -> Env
pushOuter' env vs = foldl pushOuter env (reverse vs)

-- Inner Value
type InnerClosure = Closure InnerTerm

data InnerNeutral
  = INVar LevelId
  | INApp InnerNeutral (NonEmpty InnerVal)
  | INNatElim InnerNeutral InnerClosure InnerClosure
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
type OuterClosure = Closure OuterTerm

type TeleClosure = Closure Telescope

data VTelescope
  = TVNil
  | TVCons InnerTyVal TeleClosure
  deriving (Show, Eq)

data OuterNeutral
  = ONVar LevelId
  | ONApp OuterNeutral (NonEmpty OuterVal)
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
