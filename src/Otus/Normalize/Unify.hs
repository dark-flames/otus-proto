module Otus.Normalize.Unify (
  Entry (..),
  UnifyEnv (..),
  UnifyResult (..),
  UnifyMonad,
  runUnifyMonad,
  liftEval,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Strict

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Eval (evaluateApp, evaluateNeutral)
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
  = Consistant r
  | Conflict

instance Functor UnifyResult where
  fmap f (Consistant r) = Consistant (f r)
  fmap _ Conflict = Conflict

instance Applicative UnifyResult where
  pure = Consistant
  Consistant f <*> Consistant r = Consistant (f r)
  _ <*> _ = Conflict

instance Monad UnifyResult where
  Consistant r >>= k = k r
  Conflict >>= _ = Conflict

type UnifyMonad = StateT UnifyEnv (EvalResultT UnifyResult)

runUnifyMonad :: UnifyMonad a -> UnifyEnv -> EvalResult (UnifyResult (a, UnifyEnv))
runUnifyMonad m env =
  case runResultT (runStateT m env) of
    Conflict -> Success Conflict
    Consistant r ->
      case r of
        Failure err -> Failure err
        Success x -> Success (Consistant x)

liftEval :: EvalResult a -> UnifyMonad a
liftEval eval = lift $ ResultT (pure eval)

conflict :: UnifyMonad a
conflict = lift $ ResultT Conflict

-- Problem Set
data UConstraint = UConstraint
  { ctxLift :: Int,
    cLhs :: Value,
    cRhs :: Value
  }

type ProblemSeg = Seq UConstraint

data ProblemSet = PSet
  { pBaseLvl :: LevelId,
    segs :: ProblemSeg
  }

force :: Value -> UnifyMonad Value
force = \case
  Neutral (NVar lvl) spine -> do
    entry <- findEntry lvl
    case entry of
      MetaVar (Just solution) -> liftEval $ evaluateNeutral solution spine
      ConstraintVar (Just eqRefl) -> liftEval $ evaluateNeutral eqRefl spine
      _ -> return $ Neutral (NVar lvl) spine
  val -> return val

unifySpine :: LevelId -> Spine -> Spine -> UnifyMonad ProblemSeg
unifySpine = undefined

unifyTm :: LevelId -> Value -> Value -> UnifyMonad ProblemSeg
unifyTm lvl lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (VPi lDom lHOAS, VPi rDom rHOAS) -> do
      domSegs <- unifyTm lvl lDom rDom
      let lvlVar = vvar lvl
      lCod <- liftEval $ evalHOAS lHOAS (pushEnv lvlVar)
      rCod <- liftEval $ evalHOAS rHOAS (pushEnv lvlVar)
      codSegs <- unifyTm (incrLvl lvl) lCod rCod
      return $ domSegs >< codSegs
    (VId lTy lLhs lRhs, VId rTy rLhs rRhs) -> do
      tySegs <- unifyTm lvl lTy rTy
      lhsSegs <- unifyTm lvl lLhs rLhs
      rhsSegs <- unifyTm lvl lRhs rRhs
      return $ tySegs >< lhsSegs >< rhsSegs
    (VType i, VType j) -> if i == j then return Empty else conflict
    (VLam bodyHOAS, _) -> do
      let lvlVar = vvar lvl
      lBody <- liftEval $ evalHOAS bodyHOAS (pushEnv lvlVar)
      rBody <- liftEval $ evaluateApp rhs' lvlVar
      unifyTm (incrLvl lvl) lBody rBody
    (_, VLam bodyHOAS) -> do
      let lvlVar = vvar lvl
      lBody <- liftEval $ evaluateApp lhs' lvlVar
      rBody <- liftEval $ evalHOAS bodyHOAS (pushEnv lvlVar)
      unifyTm (incrLvl lvl) lBody rBody
    (Neutral (NSplicing l) lSpine, Neutral (NSplicing r) rSpine) ->
      if l == r then
        unifySpine lvl lSpine rSpine
      else
        conflict -- todo
    (Neutral (NVar l) lSpine, Neutral (NVar r) rSpine) -> do
      lEntry <- findEntry l
      rEntry <- findEntry r
      case (lEntry, rEntry) of
        (EnvVar, EnvVar) ->
          if l == r then
            unifySpine lvl lSpine rSpine
          else
            conflict
        (MetaVar _, MetaVar _) -> undefined
        (ConstraintVar _, ConstraintVar _) -> undefined
        (LocalVar, LocalVar) ->
          if l == r then
            unifySpine lvl lSpine rSpine
          else
            conflict
        (MetaVar _, _) -> undefined
        (_, MetaVar _) -> undefined
        _ -> conflict
    (Neutral (NVar l) lSpine, _) -> undefined
    (_, Neutral (NVar r) rSpine) -> undefined
    _ -> conflict
