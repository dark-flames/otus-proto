module Otus.Normalize.Unify.State (
  LevelSet,
  Problem (..),
  Equation (..),
  EquationGroup (..),
  ProblemSeg (..),
  Entry (..),
  buildProblem,
  findEntry,
  isMetaEntry,
  isLocalEntry,
  solveMeta,
  setEquations,
  setMaskLvl,
  incrMaskLvl,
  readSolveResultRecord,
  UnifyResult (..),
  UnifyMonad,
  initProblem,
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
import qualified Data.Set as Set

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Value

data Entry
  = EnvVar
  | MetaVar (Maybe (Term, Value))
  | ConstraintProof (Maybe (Term, Value))
  | LocalVar

-- Problem Set

type LevelSet = Set.Set LevelId

data Equation = Equation
  { eLvl :: LevelId,
    eLhs :: Value,
    eRhs :: Value,
    stuckOn :: LevelSet
  }

data EquationGroup = EqGroup
  { groupLvl :: LevelId,
    equations :: Seq Equation,
    eqProof :: (Term, Value)
  }

data ProblemSeg
  = MetaDef (Maybe (Term, Value))
  | Equations EquationGroup

data Problem = Problem
  { baseLvl :: LevelId,
    problemSegs :: Seq ProblemSeg,
    maskLvl :: LevelId
  }

instance Semigroup Problem where
  l <> r =
    Problem
      { baseLvl = baseLvl l,
        problemSegs = problemSegs l <> problemSegs r,
        maskLvl = maskLvl l
      }

buildProblem :: LevelId -> VConstraint -> Problem
buildProblem lvl cstr =
  let (mask, segs) = buildSegs cstr
  in Problem lvl segs mask
  where
    buildSegs = \case
      VCstrEmpty -> (lvl, Empty)
      VCstrDef prev _ ->
        let (sLvl, prevSegs) = buildSegs prev
        in ( incrLvl sLvl,
             prevSegs |> MetaDef Nothing
           )
      VCstrTmEq prev localTele lhs rhs _ ->
        let
          (sLvl, prevSegs) = buildSegs prev
          localLvl = shift (size localTele) sLvl
          absN = size localTele
        in
          ( incrLvl sLvl,
            prevSegs
              |> ( Equations $
                     EqGroup
                       { groupLvl = sLvl,
                         equations = singleton (Equation localLvl lhs rhs mempty),
                         eqProof = (lamN absN Refl, absRefl absN)
                       }
                 )
          )

findEntry :: LevelId -> UnifyMonad Entry
findEntry lvl = do
  prob <- get
  if lvl < baseLvl prob then
    return EnvVar
  else
    if lvl < maskLvl prob then case problemSegs prob @? sub lvl (baseLvl prob) of
      Just seg -> case seg of
        Equations group ->
          ConstraintProof
            <$> if null $ equations group then
              return (Just $ eqProof group)
            else
              return Nothing
        MetaDef s -> return $ MetaVar s
      Nothing -> throwError $ Anyhow "impossible"
    else
      return LocalVar

solveMeta :: LevelId -> (Term, Value) -> UnifyMonad ()
solveMeta lvl s = do
  prob <- get
  if lvl < baseLvl prob then
    throwError $ Anyhow "Cannot solve env var"
  else do
    let idx = sub lvl (baseLvl prob)
    newSegs <- case problemSegs prob @? idx of
      Nothing -> throwError $ Anyhow "Unknown seg"
      Just (MetaDef Nothing) -> return $ Seq.update idx (MetaDef $ Just s) (problemSegs prob)
      Just (MetaDef _) -> throwError $ Anyhow "Cannot solve twice"
      Just (Equations _) -> throwError $ Anyhow "Cannot solve tm eq"
    put $ prob {problemSegs = newSegs}

setEquations :: LevelId -> Seq Equation -> UnifyMonad ()
setEquations lvl eqs = do
  prob <- get
  if lvl < baseLvl prob then
    throwError $ Anyhow "Cannot solve env var"
  else do
    let idx = sub lvl (baseLvl prob)
    newSegs <- case problemSegs prob @? idx of
      Nothing -> throwError $ Anyhow "Unknown seg"
      Just (MetaDef _) -> throwError $ Anyhow "Cannot set eqGroup of meta def"
      Just (Equations group) -> return $ Seq.update idx (Equations group {equations = eqs}) (problemSegs prob)
    put $ prob {problemSegs = newSegs}

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
setMaskLvl lvl = modify (\e -> Problem (baseLvl e) (problemSegs e) lvl)

incrMaskLvl :: UnifyMonad ()
incrMaskLvl = modify (\e -> Problem (baseLvl e) (problemSegs e) (incrLvl $ maskLvl e))

readSolveResultRecord :: UnifyMonad VRecord
readSolveResultRecord = do
  prob <- get
  let e = trivalEnv (baseLvl prob)
  go e (problemSegs prob)
  where
    go env = \case
      Empty -> return Empty
      seg :<| rst -> case seg of
        MetaDef (Just (tm, _)) -> do
          v <- liftEval $ evaluateTerm tm env
          vRst <- go (pushEnv v env) rst
          return $ v :<| vRst
        Equations group ->
          if null $ equations group then do
            let v = snd $ eqProof group
            vRst <- go (pushEnv v env) rst
            return $ v :<| vRst
          else
            conflict
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

type UnifyMonad = StateT Problem (EvalResultT UnifyResult)

emptyProblem :: LevelId -> Problem
emptyProblem lvl = Problem lvl Empty (incrLvl lvl)

initProblem :: LevelId -> VConstraint -> UnifyMonad ()
initProblem lvl cstr = put $ buildProblem lvl cstr

runUnifyMonad :: UnifyMonad a -> Problem -> EvalResult (UnifyResult (a, Problem))
runUnifyMonad m prob =
  case runResultT (runStateT m prob) of
    Conflict -> Success Conflict
    Consistent r ->
      case r of
        Failure err -> Failure err
        Success x -> Success (Consistent x)

execUnifyMonad :: UnifyMonad a -> Problem -> EvalResult (UnifyResult a)
execUnifyMonad m prob = fmap fst <$> runUnifyMonad m prob

execConv :: LevelId -> UnifyMonad () -> EvalResult Bool
execConv lvl m = f <$> execUnifyMonad m (emptyProblem lvl)
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
      MetaVar (Just (_, solution)) -> liftEval $ evaluateNeutral solution spine
      ConstraintProof (Just (_, eqRefl)) -> liftEval $ evaluateNeutral eqRefl spine
      _ -> return $ Neutral (NVar lvl) spine
  val -> return val
