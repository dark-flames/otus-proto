{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Value (..),
  Environment (..),
  envLevel,
  pushFreshVar,
  pushFreshVarN,
  pushFreshVar',
  pushFreshVarN',
  find,
  normalizeEnv,
) where

import Otus.Ast
import Otus.Common

class Value val where
  type Neutral val
  vVar :: LevelId -> val
  vVars :: (Item l ~ LevelId, Item r ~ val, Sequence l, Sequence r) => l -> r
  vVars = seqMap vVar
  fromNeutral :: Neutral val -> val

newtype Environment val = Env (Seq val)
  deriving (Eq, Show)

instance SeqSize (Environment val) where
  size (Env s) = size s

instance Sequence (Environment val) where
  type Item (Environment val) = val

  fromList :: [Item (Environment val)] -> Environment val
  fromList = Env . fromList
  toSeq (Env s) = s
  fromSeq = Env

find :: (SeqIndex id) => id -> Environment val -> Maybe val
find idx (Env e) = e @? idx

envLevel :: Environment val -> LevelId
envLevel = LevelId . size

pushFreshVar :: (Value val) => Environment val -> Environment val
pushFreshVar = snd . pushFreshVar'

pushFreshVarN :: (Value val) => Int -> Environment val -> Environment val
pushFreshVarN n env = snd (pushFreshVarN' n env)

pushFreshVar' :: (Value val) => Environment val -> (val, Environment val)
pushFreshVar' env =
  let val = (vVar $ LevelId (size env))
  in (val, env |> val)

pushFreshVarN' :: (Value val) => Int -> Environment val -> (Seq val, Environment val)
pushFreshVarN' n env =
  let
    base = LevelId (size env)
    vals = vVars $ levelRng base n
  in
    (vals, env >< vals)

normalizeEnv :: (SeqSize l, Value val) => l -> Environment val
normalizeEnv l = pushFreshVarN (size l) empty
