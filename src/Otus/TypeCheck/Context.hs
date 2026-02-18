module Otus.TypeCheck.Context (
  ContextSeg (..),
  Context (..),
  ContextTy (..),
  ctxLvl,
  emptyCtx,
  pushVTeleSeq,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize

data ContextSeg
  = MetaTy MetaValue
  | ObjTy Value

data Context = Context
  { tys :: Seq ContextSeg,
    ctxEnv :: Environment
  }

class ContextTy t where
  intoSeg :: t -> (ContextSeg, Environment -> Environment)

  (|:>) :: Context -> t -> Context
  ctx |:> t = Context (tys ctx |> seg) (f $ ctxEnv ctx)
    where
      (seg, f) = intoSeg t

instance Sized Context where
  size = size . tys

instance Indexable Context where
  type Item Context = ContextSeg
  (@?) ctx i = tys ctx @? i

instance ContextTy MetaValue where
  intoSeg t = (MetaTy t, f)
    where
      f env = env ||> mvvar (envLevel env)

instance ContextTy Value where
  intoSeg t = (ObjTy t, f)
    where
      f env = env ||> vvar (envLevel env)

ctxLvl :: Context -> LevelId
ctxLvl = envLevel . ctxEnv

emptyCtx :: Context
emptyCtx = Context mempty emptyEnv

pushVTeleSeq :: VTeleSequence -> Context -> Context
pushVTeleSeq s = go (unVTele s)
  where
    go Empty ctx = ctx
    go (h :<| rst) ctx = go rst (ctx |:> h)
