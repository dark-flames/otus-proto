module Otus.Ast.Id (
  IndexId (..),
  LevelId (..),
  CtxIndex (..),
  incrLvl,
  levelRng,
) where

import Otus.Common.Iter

newtype IndexId = IndexId Int
  deriving (Eq, Ord, Show)

newtype LevelId = LevelId Int
  deriving (Eq, Ord, Show)

class CtxIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  intoLevelInt :: (Sized a) => a -> id -> Int
  intoIndexInt :: (Sized a) => a -> id -> Int

  intoLevel :: (Sized a) => a -> id -> LevelId
  intoLevel e i = LevelId $ intoLevelInt e i

  intoIndex :: (Sized a) => a -> id -> IndexId
  intoIndex e i = IndexId $ intoIndexInt e i

instance CtxIndex IndexId where
  shift s (IndexId i) = IndexId $ i + s
  sub (IndexId i) (IndexId j) = j - i
  intoLevelInt ctx (IndexId i) = size ctx - i - 1
  intoIndexInt _ (IndexId i) = i

instance CtxIndex LevelId where
  shift s (LevelId i) = LevelId $ i + s
  sub (LevelId i) (LevelId j) = i - j
  intoLevelInt _ (LevelId i) = i
  intoIndexInt ctx (LevelId i) = size ctx - i - 1

incrLvl :: LevelId -> LevelId
incrLvl = shift 1

levelRng :: LevelId -> Int -> [LevelId]
levelRng base rng = map (`shift` base) [0 .. rng]
