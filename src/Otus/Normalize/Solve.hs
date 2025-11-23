module Otus.Normalize.Solve (
  solveSignature,
) where

import Control.Monad (when)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
-- import {-# SOURCE #-} Otus.Normalize.Eval

import Data.Foldable (Foldable (toList), foldlM)

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Value

data CachedClosure
  = Unevaluated Closure
  | Evaluated Closure Value
  deriving (Eq, Show)

asClosure :: CachedClosure -> Closure
asClosure = \case
  Unevaluated cls -> cls
  Evaluated cls _ -> cls

data MetaState
  = MSUnknown LevelId
  | MSGuarded LevelId CachedClosure (Seq.Seq VConstraint)
  | MSSolved LevelId CachedClosure
  deriving (Eq, Show)

type MetaStateSeq = Seq.Seq MetaState

data Problem = Problem LevelId MetaStateSeq
  deriving (Eq, Show)

adjustState :: LevelId -> (MetaState -> MetaState) -> Problem -> Problem
adjustState idx h (Problem base s) = Problem base $ Seq.adjust h (sub idx base) s

type SolveMonad = StateT Problem EvalResult

data SolveResult
  = NoModification
  | Modified LevelId
  | Conflict

instance Semigroup SolveResult where
  Conflict <> _ = Conflict
  _ <> Conflict = Conflict
  NoModification <> r = r
  l <> NoModification = l
  Modified lvlL <> Modified lvlR = Modified $ min lvlL lvlR

andThen :: SolveResult -> SolveMonad SolveResult -> SolveMonad SolveResult
andThen = \case
  Conflict -> const $ return Conflict
  NoModification -> id
  curRes -> fmap (curRes <>)

notConflict :: SolveResult -> Bool
notConflict = \case
  Conflict -> False
  _ -> True

fromVSig :: LevelId -> VSignature -> Problem
fromVSig lvl (VSig defs) = Problem lvl $ go lvl (Seq.fromList defs)
  where
    go _ Seq.Empty = Seq.Empty
    go l (def Seq.:<| rest) = case def of
      VMUnsolved -> MSUnknown l Seq.:<| go (incrLvl l) rest
      VMGuarded cls constrs -> MSGuarded l (Unevaluated cls) (Seq.fromList constrs) Seq.:<| go (incrLvl l) rest
      VMSolved cls -> MSSolved l (Unevaluated cls) Seq.:<| go (incrLvl l) rest

toVSig :: Problem -> VSignature
toVSig (Problem _ states) = VSig $ toList (f <$> states)
  where
    f = \case
      MSUnknown _ -> VMUnsolved
      MSGuarded _ cached constrs -> VMGuarded (asClosure cached) (toList constrs)
      MSSolved _ cached -> VMSolved (asClosure cached)

-- Control
getMetaState :: LevelId -> SolveMonad MetaState
getMetaState lvl = gets (\(Problem base s) -> Seq.index s $ sub lvl base)

getBaseLevel :: SolveMonad LevelId
getBaseLevel = gets (\(Problem base _) -> base)

getSize :: SolveMonad Int
getSize = gets (\(Problem _ s) -> size s)

assignSolvedMeta :: LevelId -> CachedClosure -> SolveMonad ()
assignSolvedMeta lvl cls =
  modify (adjustState lvl (const $ MSSolved lvl cls))

-- Solve
solveSignature :: LevelId -> VSignature -> EvalResult (Maybe VSignature)
solveSignature lvl vSig = do
  let
    problem = fromVSig lvl vSig
  (noConflict, problem') <- runStateT doSolve problem
  if noConflict then
    return $ Just $ toVSig problem'
  else
    return Nothing

doSolve :: SolveMonad Bool
doSolve = notConflict <$> (getBaseLevel >>= go)
  where
    -- run solve step from the boundary until no modification
    go lvl = do
      r <- solveOnceFrom lvl
      case r of
        Modified bound -> go bound
        _ -> return r

--- solve defs from the given level once time
solveOnceFrom :: LevelId -> SolveMonad SolveResult
solveOnceFrom lvl = getSize >>= go lvl
  where
    go _ 0 = return NoModification
    go l fuel = solveSingle l >>= (`andThen` go (incrLvl l) (fuel - 1))

solveSingle :: LevelId -> SolveMonad SolveResult
solveSingle lvl =
  getMetaState lvl >>= \case
    MSGuarded _ cls constrs -> do
      (simplified, res) <- solveConstraints constrs
      when (null simplified) $ assignSolvedMeta lvl cls
      return $ res <> Modified lvl
    _ -> return NoModification

solveConstraints :: Seq.Seq VConstraint -> SolveMonad (Seq.Seq VConstraint, SolveResult)
solveConstraints constrs = do
  (simplified, res) <- solveConstraintsOnce constrs
  case res of
    Modified _ -> do
      (simplified', res') <- solveConstraints simplified
      return (simplified', res <> res')
    _ -> return (simplified, res)

--- solve given constraints
--- effect: update previous meta state if any meta was solved
solveConstraintsOnce :: Seq.Seq VConstraint -> SolveMonad (Seq.Seq VConstraint, SolveResult)
solveConstraintsOnce = foldlM go (Seq.Empty, NoModification)
  where
    go (simplified, res) constr =
      if notConflict res then do
        (constrs, res') <- solveConstraint constr
        return (simplified Seq.>< constrs, res <> res')
      else
        return (simplified Seq.:|> constr, res)

solveConstraint :: VConstraint -> SolveMonad (Seq.Seq VConstraint, SolveResult)
solveConstraint (VTmEq vtele lhs rhs) = solveTmEq vtele (lhs, rhs)
solveConstraint _ = undefined

solveTmEq :: VTelescope -> (Value, Value) -> SolveMonad (Seq.Seq VConstraint, SolveResult)
solveTmEq _vtele = \case
  _ -> undefined
