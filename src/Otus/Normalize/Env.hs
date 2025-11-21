{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Environment (..),
  GuardedEnvironment (..),
  push,
  push',
  pushFreshVar,
  pushGuarded,
  assignMeta,
  intoEnv,
  fromEnv,
) where

import Data.Maybe (fromMaybe)
import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

import qualified Data.Sequence as Seq

newtype Environment = Env (Seq.Seq Value)
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (Env vals) = length vals

instance CtxLike Environment Value where
  findByLevel :: Environment -> Int -> Maybe Value
  findByLevel (Env vals) i = vals Seq.!? i

data GuardedEnvironment = GEnv
  { prevEnv :: Environment,
    metaEnv :: Seq.Seq (Maybe Value)
  }
  deriving (Eq, Show)

instance Contextual GuardedEnvironment where
  ctxLength (GEnv prev meta) = ctxLength prev + length meta

instance CtxLike GuardedEnvironment Value where
  findByLevel :: GuardedEnvironment -> Int -> Maybe Value
  findByLevel (GEnv prev meta) i =
    if i < prevL then
      findByLevel prev i
    else
      fmap (fromMaybe (vVar $ LevelId i)) (meta Seq.!? (i - prevL))
    where
      prevL = ctxLength prev

push :: Environment -> Value -> Environment
push (Env e) val = Env $ e Seq.|> val

push' :: Environment -> [Value] -> Environment
push' (Env e) vals = Env $ e Seq.>< Seq.fromList vals

pushFreshVar :: Environment -> Environment
pushFreshVar env = push env (freshVar env)

pushGuarded :: GuardedEnvironment -> VGuardedSubstSeg -> GuardedEnvironment
pushGuarded (GEnv prev meta) seg =
  GEnv
    { prevEnv = prev,
      metaEnv = meta Seq.|> lookupGSeg seg
    }

assignMeta :: GuardedEnvironment -> LevelId -> Value -> GuardedEnvironment
assignMeta env@(GEnv prev meta) (LevelId i) val =
  if i < ctxLength prev then
    env
  else
    GEnv prev $ Seq.update (i - ctxLength prev) (Just val) meta

lookupGSeg :: VGuardedSubstSeg -> Maybe Value
lookupGSeg VUnsolved = Nothing
lookupGSeg (VSolved val constraints) =
  if null constraints then
    Just val
  else
    Nothing

intoEnv :: GuardedEnvironment -> Environment
intoEnv (GEnv (Env prev) meta) = Env $ prev Seq.>< go (length prev) meta
  where
    go i = \case
      Seq.Empty -> Seq.empty
      seg Seq.:<| segs -> fromMaybe (vVar $ LevelId i) seg Seq.<| go (1 + i) segs

fromEnv :: Environment -> GuardedEnvironment
fromEnv env =
  GEnv
    { prevEnv = env,
      metaEnv = Seq.empty
    }
