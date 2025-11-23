module Otus.Normalize.Env (
  metaViewIntoItem,
  Environment (..),
  push,
  push',
  pushVSubst,
  envLevel,
  pushFreshVar,
  pushFreshVar',
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

newtype Environment = Env (Seq.Seq Value)
  deriving (Eq, Show)

instance Sized Environment where
  size (Env vals) = length vals

-- raw operations
push :: Value -> Environment -> Environment
push val (Env e) = Env $ e Seq.|> val

push' :: [Value] -> Environment -> Environment
push' val (Env e) = Env $ e Seq.>< Seq.fromList val

find :: (CtxIndex id) => id -> Environment -> Maybe Value
find idx (Env e) = e Seq.!? intoLevelInt e idx

-- basic operations
envLevel :: Environment -> LevelId
envLevel = LevelId . size

pushVSubst :: VSubstitution -> Environment -> Environment
pushVSubst (VSubst vals) = push' vals

pushFreshVar :: Environment -> Environment
pushFreshVar = snd . pushFreshVar'

pushFreshVar' :: Environment -> (Value, Environment)
pushFreshVar' env =
  let
    val = (vVar $ LevelId (size env))
  in
    (val, push val env)

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
