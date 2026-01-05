module Otus.Normalize.Env (
  Value (..),
  Environment (..),
) where

import Otus.Ast
import Otus.Common

class Value val where
  type Neutral val
  vVar :: LevelId -> val
  vVars :: (Item l ~ LevelId, Sequence l) => l -> Seq val
  vVars = seqMap vVar
  fromNeutral :: Neutral val -> val

class (SeqSize env, Value (Element env)) => Environment env where
  type Element env
  eempty :: env

  find :: (SeqIndex id) => id -> env -> Maybe (Element env)

  envLevel :: env -> LevelId

  push :: Element env -> env -> env
  push e = pushN (asSeq [e])

  pushN :: (Item l ~ Element env, Sequence l) => l -> env -> env

  pushFreshVar :: env -> (Element env, env)
  pushFreshVar env =
    let (s, env') = pushFreshVarN 1 env
    in (index s 0, env')

  pushFreshVarN :: Int -> env -> (Seq (Element env), env)
  pushFreshVarN n env =
    let
      base = LevelId (size env)
      vals = vVars $ levelRng base n
    in
      (vals, pushN vals env)

  pushFreshVar' :: env -> env
  pushFreshVar' = snd . pushFreshVar

  pushFreshVarN' :: Int -> env -> env
  pushFreshVarN' n = snd . pushFreshVarN n
