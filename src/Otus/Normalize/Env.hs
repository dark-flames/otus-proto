{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Env (
  Environment (..),
  push,
  push',
  pushFreshVar,
  pushVPSubst,
) where

import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

data Environment = Env
  { prevEnv :: Maybe Environment,
    metaEnv :: [Maybe Value],
    currentEnv :: [Value]
  }
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (Env prev meta cur) =
    maybe 0 ctxLength prev
      + length meta
      + length cur

instance CtxLike Environment Value where
  findByIndex :: Environment -> Int -> Maybe Value
  findByIndex e@(Env prev meta inner) i
    | i < length inner = Just $ inner !! i
    | i - length inner < length meta = case meta !! (i - length inner) of
        Just val -> Just val
        Nothing -> Just $ vMetaVar $ intoLevel e (IndexId i)
    | otherwise = case prev of
        Just p ->
          mapMetaVar
            <$> findByIndex p (i - length inner - length meta)
        Nothing -> Nothing

push :: Environment -> Value -> Environment
push (Env prev meta inner) val =
  Env
    { prevEnv = prev,
      metaEnv = meta,
      currentEnv = val : inner
    }

push' :: Environment -> [Value] -> Environment
push' = foldl push

pushFreshVar :: Environment -> Environment
pushFreshVar env = push env (freshVar env)

pushVPSubst :: Environment -> VPartialSubstitution -> Environment
pushVPSubst (Env prev meta []) pSubst =
  Env
    { prevEnv = prev,
      metaEnv = vPSubstToList pSubst ++ meta,
      currentEnv = []
    }
pushVPSubst prev pSubst =
  Env
    { prevEnv = Just prev,
      metaEnv = vPSubstToList pSubst,
      currentEnv = []
    }
