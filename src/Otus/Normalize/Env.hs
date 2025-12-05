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
) where

import Otus.Ast
import Otus.Common

class Value val where
  vVar :: LevelId -> val

newtype Environment val = Env (Seq val)
  deriving (Eq, Show)

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
    vals = fromList $ map vVar $ levelRng base n
  in
    (vals, env >< vals)
