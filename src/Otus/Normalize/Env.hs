module Otus.Normalize.Env (
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
import Otus.Normalize.Value

newtype Environment = Env ValueSeq
  deriving (Eq, Show)

instance Sequence Environment where
  type Item Environment = Value

  fromList = Env . fromList
  toSeq (Env s) = s
  fromSeq = Env

find :: (SeqIndex id) => id -> Environment -> Maybe Value
find idx (Env e) = e @? idx

envLevel :: Environment -> LevelId
envLevel = LevelId . size

pushFreshVar :: Environment -> Environment
pushFreshVar = snd . pushFreshVar'

pushFreshVarN :: Int -> Environment -> Environment
pushFreshVarN n env = snd (pushFreshVarN' n env)

pushFreshVar' :: Environment -> (Value, Environment)
pushFreshVar' env =
  let val = (vVar $ LevelId (size env))
  in (val, env |> val)

pushFreshVarN' :: Int -> Environment -> (ValueSeq, Environment)
pushFreshVarN' n env =
  let
    base = LevelId (size env)
    vals = fromList $ map vVar $ levelRng base n
  in
    (vals, env >< vals)
