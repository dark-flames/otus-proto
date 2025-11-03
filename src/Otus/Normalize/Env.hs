module Otus.Normalize.Env (
  Environment (..),
  envStage,
) where

import Data.List ((!?))
import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

data Environment
  = ObjEnv [Value]
  | MetaEnv
      { outerEnv :: [Value],
        metaEnv :: [Maybe Value],
        innerEnv :: [Value]
      }
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (ObjEnv vals) = length vals
  ctxLength (MetaEnv outer meta inner) = length outer + length meta + length inner

instance CtxLike Environment Value where
  findByIndex (ObjEnv vals) i = vals !? i
  findByIndex e@(MetaEnv outer meta inner) i
    | i < length inner = Just $ inner !! i
    | i - length inner < length meta = case meta !! (i - length inner) of
        Just val -> Just val
        Nothing -> Just $ vMetaVar $ intoLevel e $ IndexId i
    | otherwise = outer !? (i - length inner - length meta)

  push (ObjEnv vals) val = ObjEnv (val : vals)
  push (MetaEnv outer meta inner) val =
    MetaEnv
      { outerEnv = outer,
        metaEnv = meta,
        innerEnv = val : inner
      }

envStage :: Environment -> Stage
envStage (ObjEnv _) = Object
envStage (MetaEnv {}) = Meta
