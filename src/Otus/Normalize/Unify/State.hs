module Otus.Normalize.Unify.State (
  Entry (..),
  UnifyEnv (..),
  findEntry,
  isMetaEntry,
  isLocalEntry,
  setEntry,
  setMaskLvl,
  incrMaskLvl,
  readSolveResultRecord,
  UnifyResult (..),
  UnifyMonad,
  emptyUnifyEnv,
  initUnifyEnv,
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
import Control.Monad.State.Lazy

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Value

data Entry
  = EnvVar
  | MetaVar (Maybe (Value, Term))
  | ConstraintProof (Maybe Value)
  | LocalVar

data UnifyEnv = UnifyEnv
  { baseLvl :: LevelId,
    entries :: Seq Entry,
    maskLvl :: LevelId
  }

findEntry :: LevelId -> UnifyMonad Entry
findEntry lvl = do
  uEnv <- get
  if lvl < baseLvl uEnv then
    return EnvVar
  else
    if lvl < maskLvl uEnv then case entries uEnv @? sub lvl (baseLvl uEnv) of
      Just e -> return e
      Nothing -> throwError $ Anyhow "impossible"
    else
      return LocalVar

setEntry :: LevelId -> Entry -> UnifyMonad ()
setEntry lvl entry = do
  uEnv <- get
  if lvl < baseLvl uEnv then
    throwError $ Anyhow "Cannot set env var"
  else do
    let idx = sub lvl (baseLvl uEnv)
    let newEntries = Seq.update idx entry (entries uEnv)
    put (UnifyEnv (baseLvl uEnv) newEntries (maskLvl uEnv))

isMetaEntry :: LevelId -> UnifyMonad Bool
isMetaEntry lvl =
  findEntry lvl >>= \case
    MetaVar _ -> return True
    _ -> return False

isLocalEntry :: LevelId -> UnifyMonad Bool
isLocalEntry lvl =
  findEntry lvl >>= \case
    LocalVar -> return True
    _ -> return False

setMaskLvl :: LevelId -> UnifyMonad ()
setMaskLvl lvl = modify (\e -> UnifyEnv (baseLvl e) (entries e) lvl)

incrMaskLvl :: UnifyMonad ()
incrMaskLvl = modify (\e -> UnifyEnv (baseLvl e) (entries e) (incrLvl $ maskLvl e))

readSolveResultRecord :: UnifyMonad VRecord
readSolveResultRecord = do
  uEnv <- get
  let e = trivalEnv (baseLvl uEnv)
  go e (entries uEnv)
  where
    go env = \case
      Empty -> return Empty
      entry :<| rst -> case entry of
        MetaVar (Just (_, tm)) -> do
          v <- liftEval $ evaluateTerm tm env
          vRst <- go (pushEnv v env) rst
          return $ v :<| vRst
        ConstraintProof (Just v) -> do
          vRst <- go (pushEnv v env) rst
          return $ v :<| vRst
        _ -> conflict

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
emptyUnifyEnv lvl = UnifyEnv lvl Empty lvl

initUnifyEnv :: LevelId -> VProblem -> UnifyMonad ()
initUnifyEnv lvl prob = put $ UnifyEnv lvl envEntries lvl
  where
    constrToEntry = \case
      VTmEq {} -> ConstraintProof Nothing
      VMetaDef _ -> MetaVar Nothing
    envEntries = fmap constrToEntry prob

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
      MetaVar (Just (solution, _)) -> liftEval $ evaluateNeutral solution spine
      ConstraintProof (Just eqRefl) -> liftEval $ evaluateNeutral eqRefl spine
      _ -> return $ Neutral (NVar lvl) spine
  val -> return val
