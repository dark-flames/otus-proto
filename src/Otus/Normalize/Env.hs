{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Environment (..),
  push,
  push',
  pushFreshVar,
  pushMetaDef,
  lensIterM,
  MetaEnv (..),
  VarKind (..),
  varKind,
  assignMeta,
  lookupMeta,
  updateMeta,
) where

import Data.Maybe (fromMaybe)

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

push :: Value -> Environment -> Environment
push val (Env e) = Env $ e Seq.|> val

push' :: [Value] -> Environment -> Environment
push' vals (Env e) = Env $ e Seq.>< Seq.fromList vals

pushFreshVar :: Environment -> Environment
pushFreshVar env = push (freshVar env) env

pushMetaDef :: VMetaDefinition -> Environment -> Environment
pushMetaDef def = maybe id push $ metaView def

lensIterM
  :: (Monad m)
  => (item -> Environment -> m r)
  -> (r -> Environment -> Environment)
  -> [item]
  -> Environment
  -> m (Environment, [r])
lensIterM process updateEnv input env = case input of
  [] -> return (env, [])
  x : xs -> do
    res <- process x env
    (env', rest) <- go xs $ updateEnv res env
    return (env', res : rest)
  where
    go = lensIterM process updateEnv

data MetaEnv = MetaEnv
  { envLvl :: LevelId,
    metaDefs :: Seq.Seq (Maybe Value)
  }
  deriving (Eq, Show)

data VarKind
  = EnvVar
  | MetaVar
  | LocalVar
  | NonVar

varKind :: Value -> MetaEnv -> VarKind
varKind val (MetaEnv envL defs) = case maybeVar val of
  Just lvl
    | lvl < envL -> EnvVar
    | envL <= lvl && lvl < shift (length defs) envL -> MetaVar
    | otherwise -> LocalVar
  _ -> NonVar

assignMeta :: LevelId -> Value -> MetaEnv -> MetaEnv
assignMeta lvl val (MetaEnv envL defs) =
  let
    defId = sub lvl envL
  in
    MetaEnv
      { envLvl = envL,
        metaDefs = Seq.update defId (Just val) defs
      }

lookupMeta :: LevelId -> MetaEnv -> Maybe Value
lookupMeta lvl (MetaEnv envL defs) =
  let
    defId = sub lvl envL
  in
    fromMaybe Nothing $ defs Seq.!? defId

updateMeta :: LevelId -> VMetaDefinition -> MetaEnv -> MetaEnv
updateMeta lvl def = maybe id (assignMeta lvl) $ metaView def
