{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Environment (..),
  push,
  push',
  pushMeta,
  pushMeta',
  pushFreshVar,
  pushVPSubst,
) where

import Data.List ((!?))
import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

data Environment = Env
  { prevEnv :: Maybe Environment,
    metaEnv :: [Maybe Value],
    currentEnv :: [Value]
  }
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (Env prev meta cur) =
    maybe 0 ctxLength prev
      + length meta
      + length cur

instance CtxLike Environment Value where
  findByLevel :: Environment -> Int -> Maybe Value
  findByLevel (Env prevM meta current) i = case prevM of
    (Just prev) | i < ctxLength prev -> mapMetaVarToNormal <$> findByLevel prev i
    _ ->
      let
        prevLength = maybe 0 ctxLength prevM
      in
        if i - prevLength < length meta then case meta !! (i - prevLength) of
          Just val -> Just val
          Nothing -> Just (vMetaVar $ LevelId i)
        else
          current !? (i - prevLength - length current)

push :: Environment -> Value -> Environment
push env val = push' env [val]

push' :: Environment -> [Value] -> Environment
push' (Env prev meta inner) vals =
  Env
    { prevEnv = prev,
      metaEnv = meta,
      currentEnv = inner ++ vals
    }

pushMeta :: Environment -> Maybe Value -> Environment
pushMeta env val = pushMeta' env [val]

pushMeta' :: Environment -> [Maybe Value] -> Environment
pushMeta' (Env prev meta []) vals =
  Env
    { prevEnv = prev,
      metaEnv = meta ++ vals,
      currentEnv = []
    }
pushMeta' prev vals =
  Env
    { prevEnv = Just prev,
      metaEnv = vals,
      currentEnv = []
    }

pushFreshVar :: Environment -> Environment
pushFreshVar env = push env (freshVar env)

pushVPSubst :: Environment -> VPartialSubstitution -> Environment
pushVPSubst env = pushMeta' env . vPSubstToList
