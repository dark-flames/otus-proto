module Otus.Ast.Id (
  IndexId (..),
  LevelId (..),
  SeqIndex (..),
  IndexableSeq (..),
  incrLvl,
  levelRng,
  toLevel,
  toIndex,
) where

import Otus.Common.Iter

newtype IndexId = IndexId Int
  deriving (Eq, Ord, Show)

instance SeqIndex IndexId where
  shift s (IndexId i) = IndexId $ i - s
  sub (IndexId i) (IndexId j) = j - i
  intoLeftIndex l (IndexId i) = size l - i - 1
  intoRightIndex _ (IndexId i) = i

toLevel :: (Sized (l a)) => l a -> IndexId -> LevelId
toLevel s idx = LevelId $ intoLeftIndex s idx

newtype LevelId = LevelId Int
  deriving (Eq, Ord, Show)

instance SeqIndex LevelId where
  shift s (LevelId i) = LevelId $ i + s
  sub (LevelId i) (LevelId j) = i - j
  intoLeftIndex _ (LevelId i) = i
  intoRightIndex l (LevelId i) = size l - i - 1

incrLvl :: LevelId -> LevelId
incrLvl = shift 1

levelRng :: LevelId -> Int -> [LevelId]
levelRng base rng = map (`shift` base) [0 .. rng]

toIndex :: (Sized (l a)) => l a -> LevelId -> IndexId
toIndex s lvl = IndexId $ intoRightIndex s lvl
