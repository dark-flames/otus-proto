{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Environment (..),
  push,
  push',
  pushFreshVar,
) where

import qualified Data.Sequence as Seq

import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

newtype Environment = Env (Seq.Seq Value)
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (Env vals) = length vals

instance CtxLike Environment Value where
  findByLevel :: Environment -> Int -> Maybe Value
  findByLevel (Env vals) i = vals Seq.!? i

push :: Environment -> Value -> Environment
push (Env e) val = Env $ e Seq.|> val

push' :: Environment -> [Value] -> Environment
push' (Env e) vals = Env $ e Seq.>< Seq.fromList vals

pushFreshVar :: Environment -> Environment
pushFreshVar env = push env (freshVar env)
