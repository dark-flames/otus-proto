module Otus.Normalize.Env (
  EnvItem (..),
  itemView,
  metaViewIntoItem,
  Environment (..),
  push,
  push',
  pushVSubst,
  envLevel,
  pushFreshVar,
  pushFreshVar',
  pushMetaView,
  pushMetaView',
  updateMeta,
  assignSolvedMeta,
  find,
  findMeta,
  findSolvedMeta,
  collectArgs,
  lensIterM,
) where

import Data.Foldable (Foldable (toList))

import qualified Data.Sequence as Seq

import Otus.Ast.Id (CtxIndex (intoLevel, intoLevelInt), LevelId (..))
import Otus.Common
import Otus.Normalize.Value

data EnvItem
  = EVal Value
  | EUnsolvedMeta LevelId
  | ESolvedMeta Value
  deriving (Eq, Show)

metaViewIntoItem :: LevelId -> VMetaView -> EnvItem
metaViewIntoItem lvl = \case
  SolvedMeta val -> ESolvedMeta val
  UnsolvedMeta -> EUnsolvedMeta lvl

itemView :: EnvItem -> Value
itemView = \case
  EVal val -> val
  ESolvedMeta val -> val
  EUnsolvedMeta lvl -> vVar lvl

newtype Environment = Env (Seq.Seq EnvItem)
  deriving (Eq, Show)

instance Sized Environment where
  size (Env vals) = length vals

-- raw operations
pushRaw :: EnvItem -> Environment -> Environment
pushRaw item (Env e) = Env $ e Seq.|> item

pushRaw' :: [EnvItem] -> Environment -> Environment
pushRaw' items (Env e) = Env $ e Seq.>< Seq.fromList items

assignRaw :: (CtxIndex id) => id -> EnvItem -> Environment -> Environment
assignRaw idx item (Env e) = Env $ Seq.update (intoLevelInt e idx) item e

lookupRaw :: (CtxIndex id) => id -> Environment -> Maybe EnvItem
lookupRaw idx (Env e) = e Seq.!? intoLevelInt e idx

-- basic operations
envLevel :: Environment -> LevelId
envLevel = LevelId . size

push :: Value -> Environment -> Environment
push = pushRaw . EVal

push' :: [Value] -> Environment -> Environment
push' = pushRaw' . map EVal

pushVSubst :: VSubstitution -> Environment -> Environment
pushVSubst (VSubst vals) = push' vals

pushFreshVar :: Environment -> Environment
pushFreshVar env = push (vVar $ LevelId (size env)) env

pushFreshVar' :: Environment -> (Value, Environment)
pushFreshVar' env =
  let
    val = (vVar $ LevelId (size env))
  in
    (val, push val env)

pushMetaView :: VMetaView -> Environment -> Environment
pushMetaView view env = pushRaw (metaViewIntoItem (envLevel env) view) env

pushMetaView' :: VMetaView -> Environment -> (Value, Environment)
pushMetaView' view env =
  let
    val = metaViewIntoItem (envLevel env) view
  in
    (itemView val, pushRaw val env)

updateMeta :: (CtxIndex id) => id -> VMetaView -> Environment -> Environment
updateMeta idx view env = assignRaw idx (metaViewIntoItem (intoLevel env idx) view) env

assignSolvedMeta :: (CtxIndex id) => id -> Value -> Environment -> Environment
assignSolvedMeta idx val = assignRaw idx (ESolvedMeta val)

find :: (CtxIndex id) => id -> Environment -> Maybe Value
find idx env = itemView <$> lookupRaw idx env

findMeta :: (CtxIndex id) => id -> Environment -> Maybe VMetaView
findMeta idx env = case lookupRaw idx env of
  Just (ESolvedMeta val) -> Just $ SolvedMeta val
  Just (EUnsolvedMeta _) -> Just UnsolvedMeta
  _ -> Nothing

findSolvedMeta :: (CtxIndex id) => id -> Environment -> Maybe Value
findSolvedMeta idx env = case lookupRaw idx env of
  Just (ESolvedMeta val) -> Just val
  _ -> Nothing

collectArgs :: (CtxIndex id) => id -> Environment -> [Value]
collectArgs idx (Env e) = maybe [] (toList . fmap itemView) $ Seq.tails e Seq.!? intoLevelInt e idx

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
