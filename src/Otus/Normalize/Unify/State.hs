module Otus.Normalize.Unify.State (
  Entry (..),
  UnifyEnv (..),
  findEntry,
  UnifyResult (..),
  UnifyMonad,
  runUnifyMonad,
  execUnifyMonad,
  execConv,
  isConsistent,
  liftEval,
  conflict,
  conflictIf,
  force,
) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Strict

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Value

data Entry
  = EnvVar
  | MetaVar (Maybe Value)
  | ConstraintVar (Maybe Value)
  | LocalVar

data UnifyEnv = UnifyEnv
  { baseLvl :: LevelId,
    entries :: Seq Entry
  }

findEntry :: LevelId -> UnifyMonad Entry
findEntry lvl = do
  uEnv <- get
  let entrySize = size (entries uEnv)
  let entriesLvl = shift entrySize (baseLvl uEnv)
  if lvl < baseLvl uEnv then
    return EnvVar
  else
    if lvl < entriesLvl then case entries uEnv @? sub lvl (baseLvl uEnv) of
      Just e -> return e
      Nothing -> throwError $ Anyhow "impossible"
    else
      return LocalVar

-- Control
data UnifyResult r
  = Consistent r
  | Conflict

instance Functor UnifyResult where
  fmap f (Consistent r) = Consistent (f r)
  fmap _ Conflict = Conflict

instance Applicative UnifyResult where
  pure = Consistent
  Consistent f <*> Consistent r = Consistent (f r)
  _ <*> _ = Conflict

instance Monad UnifyResult where
  Consistent r >>= k = k r
  Conflict >>= _ = Conflict

type UnifyMonad = StateT UnifyEnv (EvalResultT UnifyResult)

emptyUnifyEnv :: LevelId -> UnifyEnv
emptyUnifyEnv lvl = UnifyEnv lvl Empty

runUnifyMonad :: UnifyMonad a -> UnifyEnv -> EvalResult (UnifyResult (a, UnifyEnv))
runUnifyMonad m env =
  case runResultT (runStateT m env) of
    Conflict -> Success Conflict
    Consistent r ->
      case r of
        Failure err -> Failure err
        Success x -> Success (Consistent x)

execUnifyMonad :: UnifyMonad a -> UnifyEnv -> EvalResult (UnifyResult a)
execUnifyMonad m env = fmap fst <$> runUnifyMonad m env

execConv :: LevelId -> UnifyMonad () -> EvalResult Bool
execConv lvl m = f <$> execUnifyMonad m (emptyUnifyEnv lvl)
  where
    f = \case
      Conflict -> False
      Consistent _ -> True

isConsistent :: UnifyMonad () -> UnifyMonad Bool
isConsistent m = do
  env <- get
  case runResultT (runStateT m env) of
    Conflict -> return False
    Consistent r ->
      case r of
        Failure err -> throwError err
        Success _ -> return True

liftEval :: EvalResult a -> UnifyMonad a
liftEval eval = lift $ ResultT (pure eval)

conflict :: UnifyMonad a
conflict = lift $ ResultT Conflict

conflictIf :: Bool -> UnifyMonad ()
conflictIf cond = when cond conflict

force :: Value -> UnifyMonad Value
force = \case
  Neutral (NVar lvl) spine -> do
    entry <- findEntry lvl
    case entry of
      MetaVar (Just solution) -> liftEval $ evaluateNeutral solution spine
      ConstraintVar (Just eqRefl) -> liftEval $ evaluateNeutral eqRefl spine
      _ -> return $ Neutral (NVar lvl) spine
  val -> return val
