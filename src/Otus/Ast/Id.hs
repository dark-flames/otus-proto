module Otus.Ast.Id (
  IndexId(..),
  LevelId(..),
  CtxLike(..),
  CtxIndex(..)
) where

newtype IndexId = IndexId Int
  deriving (Show, Eq)

newtype LevelId = LevelId Int
  deriving (Show, Eq)

class Contextual a where
  ctxLength :: a -> Int

class Contextual a => CtxLike a e where
  (!?) :: a -> Int -> Maybe e

class CtxIndex id where
  find :: CtxLike a e => a -> id -> Maybe e
  intoLevel :: Contextual a => a -> id -> LevelId
  intoIndex :: Contextual a => a -> id -> IndexId

instance CtxIndex IndexId where
  find ctx index = find ctx (intoLevel ctx index)
  intoLevel ctx (IndexId i) = LevelId $ ctxLength ctx - i - 1
  intoIndex _ (IndexId i) = IndexId i

instance CtxIndex LevelId where
  find ctx (LevelId i) = ctx !? i
  intoLevel _ (LevelId i) = LevelId i
  intoIndex ctx (LevelId i) = IndexId $ ctxLength ctx - i - 1
