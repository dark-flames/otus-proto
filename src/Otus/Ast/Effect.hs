module Otus.Ast.Effect (
  Effect,
  EffectSet,
  PartialOrder (..),
  JoinSemilattice (..),
  singletonEff,
) where

import Data.Set (Set, isSubsetOf, union)

import qualified Data.Set as S

import Otus.Common

data Effect
  = Unification
  | NonTermination
  deriving (Eq, Ord, Show)

newtype EffectSet = EffectSet
  { rawSet :: Set Effect
  }
  deriving (Eq, Show)

instance Semigroup EffectSet where
  l <> r = EffectSet (rawSet l <> rawSet r)

instance Monoid EffectSet where
  mempty = EffectSet mempty

instance PartialOrder EffectSet where
  cmp (EffectSet l) (EffectSet r) = case (l `isSubsetOf` r, r `isSubsetOf` l) of
    (True, True) -> Just EQ
    (True, False) -> Just LT
    (False, True) -> Just GT
    (False, False) -> Nothing

instance JoinSemilattice EffectSet where
  (EffectSet l) \/ (EffectSet r) = EffectSet $ union l r

instance Pretty Effect where
  pretty = \case
    Unification -> "u"
    NonTermination -> "t"

instance Pretty EffectSet where
  pretty s =
    if S.null $ rawSet s then
      "∅"
    else
      foldl (\l r -> l ++ "\\/" ++ r) "" $ map pretty (S.toList $ rawSet s)

singletonEff :: Effect -> EffectSet
singletonEff = EffectSet . S.singleton
