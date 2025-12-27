module Otus.Ast.Id (
  IndexId (..),
  LevelId (..),
  SeqIndex (..),
  incrLvl,
  levelRng,
  toLevel,
  toIndex,
) where

import Otus.Common

newtype IndexId = IndexId Int
  deriving (Eq, Num, Ord, Show) via Int

instance SeqIndex IndexId where
  shift s (IndexId i) = IndexId $ i - s
  sub (IndexId i) (IndexId j) = j - i
  intoLeftIndex l (IndexId i) = size l - i - 1
  intoRightIndex _ (IndexId i) = i

toLevel :: (SeqSize l) => l -> IndexId -> LevelId
toLevel s idx = LevelId $ intoLeftIndex (size s) idx

newtype LevelId = LevelId Int
  deriving (Eq, Num, Ord, Show) via Int

instance SeqIndex LevelId where
  shift s (LevelId i) = LevelId $ i + s
  sub (LevelId i) (LevelId j) = i - j
  intoLeftIndex _ (LevelId i) = i
  intoRightIndex l (LevelId i) = size l - i - 1

instance SeqSize LevelId where
  size (LevelId lvl) = lvl

incrLvl :: LevelId -> LevelId
incrLvl = shift 1

levelRng :: LevelId -> Int -> Seq LevelId
levelRng base rng = seqMap (`shift` base) $ asSeq [0 .. rng]

toIndex :: (SeqSize l) => l -> LevelId -> IndexId
toIndex l lvl = IndexId $ intoRightIndex l lvl
