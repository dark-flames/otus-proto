module Otus.Elaboration.Context (
  Context (..),
  ContextIndex (..),
) where

import qualified Data.Map as M

import Otus.Ast
import Otus.Common
import Otus.Elaboration.Judgement
import Otus.Normalize

data Context = Context
  { ctxSeq :: Seq HybridType,
    nameMap :: M.Map Name LevelId,
    ctxEnv :: Environment
  }

instance Sized Context where
  size = size . ctxSeq

class ContextIndex id where
  asLevelId :: Context -> id -> Maybe LevelId

  asIndexId :: Context -> id -> Maybe IndexId
  asIndexId ctx idx = do
    LevelId i <- asLevelId ctx idx
    if i < 0 then Nothing else Just . IndexId $ intoRightIndex ctx i

  (@!) :: Context -> id -> Maybe (IndexId, HybridType)
  ctx @! idx = do
    lvl <- asLevelId ctx idx
    i <- asIndexId ctx lvl
    ty <- ctxSeq ctx @? lvl
    return (i, ty)

instance ContextIndex Name where
  asLevelId :: Context -> Name -> Maybe LevelId
  asLevelId ctx name = nameMap ctx M.!? name

  asIndexId :: Context -> Name -> Maybe IndexId
  asIndexId ctx name = toIndex ctx <$> asLevelId ctx name

instance ContextIndex LevelId where
  asLevelId :: Context -> LevelId -> Maybe LevelId
  asLevelId ctx (LevelId i) =
    if i < 0 || i > size ctx then
      Nothing
    else
      Just (LevelId i)

  asIndexId :: Context -> LevelId -> Maybe IndexId
  asIndexId ctx (LevelId i) =
    if i < 0 || i > size ctx then
      Nothing
    else
      Just . IndexId $ intoRightIndex ctx i

instance ContextIndex IndexId where
  asLevelId :: Context -> IndexId -> Maybe LevelId
  asLevelId ctx (IndexId i) =
    if i < 0 || i > size ctx then
      Nothing
    else
      Just . LevelId $ intoLeftIndex ctx i

  asIndexId :: Context -> IndexId -> Maybe IndexId
  asIndexId ctx (IndexId i) =
    if i < 0 || i > size ctx then
      Nothing
    else
      Just (IndexId i)
