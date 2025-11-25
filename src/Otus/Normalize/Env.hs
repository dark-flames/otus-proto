module Otus.Normalize.Env (
  metaViewIntoItem,
  Environment (..),
  push,
  push',
  pushVSubst,
  envLevel,
  pushFreshVar,
  pushFreshVarN,
  pushFreshVar',
  pushFreshVarN',
  find,
  lensIterM,
) where

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import Otus.Normalize.Value

metaViewIntoItem :: LevelId -> VMetaView -> Value
metaViewIntoItem lvl = \case
  SolvedMeta val -> val
  UnsolvedMeta -> vVar lvl

newtype Environment = Env ValueSeq
  deriving (Eq, Show)

instance Sized Environment where
  size (Env vals) = length vals

-- raw operations
push :: Value -> Environment -> Environment
push val (Env e) = Env $ e Seq.|> val

push' :: ValueSeq -> Environment -> Environment
push' vals (Env e) = Env $ e Seq.>< vals

find :: (CtxIndex id) => id -> Environment -> Maybe Value
find idx (Env e) = e Seq.!? intoLevelInt e idx

-- basic operations
envLevel :: Environment -> LevelId
envLevel = LevelId . size

pushVSubst :: VSubstitution -> Environment -> Environment
pushVSubst (VSubst vals) = push' vals

pushFreshVar :: Environment -> Environment
pushFreshVar = snd . pushFreshVar'

pushFreshVarN :: Int -> Environment -> Environment
pushFreshVarN n env = snd (pushFreshVarN' n env)

pushFreshVar' :: Environment -> (Value, Environment)
pushFreshVar' env =
  let
    val = (vVar $ LevelId (size env))
  in
    (val, push val env)

pushFreshVarN' :: Int -> Environment -> (ValueSeq, Environment)
pushFreshVarN' n env =
  let
    base = LevelId (size env)
    vals = Seq.fromList $ map vVar $ levelRng base n
  in
    (vals, push' vals env)

-- iteration
lensIterM
  :: (Monad m)
  => (item -> Environment -> m r)
  -> (r -> Environment -> Environment)
  -> [item]
  -> Environment
  -> m ([r], Environment)
lensIterM process updateEnv input env = case input of
  [] -> return ([], env)
  x : xs -> do
    res <- process x env
    (rest, env') <- go xs $ updateEnv res env
    return (res : rest, env')
  where
    go = lensIterM process updateEnv
