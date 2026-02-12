module Otus.Common.Algebra (
  PartialOrder (..),
  JoinSemilattice (..),
) where

class PartialOrder a where
  cmp :: a -> a -> Maybe Ordering
  lt :: a -> a -> Maybe Bool
  lt l r = (LT ==) <$> cmp l r

  gt :: a -> a -> Maybe Bool
  gt l r = (GT ==) <$> cmp l r

  eq :: a -> a -> Maybe Bool
  eq l r = (EQ ==) <$> cmp l r

  lte :: a -> a -> Maybe Bool
  lte l r = (\o -> LT == o || EQ == o) <$> cmp l r

  gte :: a -> a -> Maybe Bool
  gte l r = (\o -> GT == o || EQ == o) <$> cmp l r

class (PartialOrder a) => JoinSemilattice a where
  (\/) :: a -> a -> a
