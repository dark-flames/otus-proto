module Otus.Ast.Id (
  IndexId (..),
  LevelId (..),
  Contextual (..),
  CtxLike (..),
  CtxIndex (..),
) where

newtype IndexId = IndexId Int
  deriving (Eq, Ord, Show)

newtype LevelId = LevelId Int
  deriving (Eq, Ord, Show)

class Contextual a where
  ctxLength :: a -> Int

class (Contextual a) => CtxLike a e where
  findByLevel :: a -> Int -> Maybe e

class CtxIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  find :: (CtxLike a e) => a -> id -> Maybe e
  intoLevel :: (Contextual a) => a -> id -> LevelId
  intoIndex :: (Contextual a) => a -> id -> IndexId

instance CtxIndex IndexId where
  shift s (IndexId i) = IndexId $ i + s
  sub (IndexId i) (IndexId j) = j - i
  find ctx index = find ctx (intoLevel ctx index)

  intoLevel ctx (IndexId i) = LevelId $ ctxLength ctx - i - 1
  intoIndex _ (IndexId i) = IndexId i

instance CtxIndex LevelId where
  shift s (LevelId i) = LevelId $ i + s
  sub (LevelId i) (LevelId j) = i - j
  find ctx (LevelId levelId) = findByLevel ctx levelId
  intoLevel _ (LevelId i) = LevelId i
  intoIndex ctx (LevelId i) = IndexId $ ctxLength ctx - i - 1
