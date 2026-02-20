module Otus.Normalize.Unify.Unify (
  solveProblem,
  unifyTm,
) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))

import qualified Data.Set as Set

import Otus.Ast
import Otus.Common
import Otus.Normalize.Error
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Unify.Conv
import Otus.Normalize.Unify.State
import Otus.Normalize.Value

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
    eqProof :: Value
  }

data EquationSet = EqSet
  { pBaseLvl :: LevelId,
    groups :: Seq EquationGroup
  }

data SolveResult = SolveResult
  { postponed :: Seq Equation,
    solvedEntries :: LevelSet
  }

instance Semigroup EquationSet where
  l <> r =
    EqSet
      { pBaseLvl = pBaseLvl l,
        groups = groups l <> groups r
      }

instance Monoid EquationSet where
  mempty = EqSet (LevelId 0) Empty

instance Semigroup SolveResult where
  l <> r = SolveResult (postponed l <> postponed r) (solvedEntries l <> solvedEntries r)

instance Monoid SolveResult where
  mempty = SolveResult Empty mempty

postpone :: Equation -> SolveResult
postpone c = SolveResult (singleton c) mempty

-- Process
buildEquationSet :: LevelId -> VProblem -> EquationSet
buildEquationSet lvl problem =
  let eqGroups = buildGroups lvl problem
  in EqSet lvl eqGroups
  where
    buildEquationGroup pLvl c@(VTmEq localTele lhs rhs _) =
      let localLvl = shift (size localTele) pLvl
      in EqGroup
           { groupLvl = pLvl,
             equations = singleton (Equation localLvl lhs rhs mempty),
             eqProof = constraintAsRefl c
           }

    buildGroups pLvl = \case
      Empty -> Empty
      c :<| rest ->
        let
          group = buildEquationGroup pLvl c
          restEqs = buildGroups (incrLvl pLvl) rest
        in
          group <| restEqs

-- | Solve one equation group:
-- | unify all equations, keep postponed ones, and update the group's proof when fully solved.
-- | Returns (isGroupSolved, updatedEquationSet, solvedEntries).
solveGroup :: EquationGroup -> UnifyMonad (Bool, EquationGroup, LevelSet)
solveGroup group = do
  let gLvl = groupLvl group
  setProblemLvl gLvl

  (leftEqs, solved) <- foldlM step (Empty, mempty) (equations group)
  let
    groupSolved = null leftEqs
    group' = group {equations = leftEqs}

  when groupSolved $
    setEntry gLvl (ConstraintProof (Just $ eqProof group))

  pure (groupSolved, group', solved)
  where
    step (carryEqs, carrySolved) (Equation lvl lhs rhs _) = do
      res <- unifyTm lvl lhs rhs
      pure (carryEqs >< postponed res, carrySolved <> solvedEntries res)

solveStep :: EquationSet -> UnifyMonad (Bool, EquationSet, LevelSet)
solveStep eqSet = do
  (finished, resGroups, solved) <- foldlM step (True, Empty, mempty) (groups eqSet)

  return (finished, eqSet {groups = resGroups}, solved)
  where
    step (allSolved, carryGroups, carrySolved) group = do
      (segSolved, resGroup, groupSolved) <- solveGroup group
      return (allSolved && segSolved, carryGroups |> resGroup, Set.union carrySolved groupSolved)

solveCycle :: EquationSet -> UnifyMonad ()
solveCycle = go
  where
    go eqSet = do
      (allSolved, nextEqSet, solved) <- solveStep eqSet
      if allSolved then
        return ()
      else
        if Set.null solved then
          conflict
        else
          go nextEqSet

solveProblem :: LevelId -> Int -> VProblem -> UnifyMonad VRecord
solveProblem lvl metaSize problem = do
  initUnifyEnv lvl metaSize (size problem)
  let pLvl = shift metaSize lvl
  let eqSet = buildEquationSet pLvl problem
  solveCycle eqSet
  readSolveResultRecord

-- Unification
solveMetaEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad SolveResult
solveMetaEntry = undefined

solveEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad SolveResult
solveEntry lvl i spine val =
  findEntry i >>= \case
    EnvVar -> conflict
    MetaVar Nothing -> solveMetaEntry lvl i spine val
    MetaVar _ -> throwError CannotSoveMetaTwice
    ConstraintProof _ -> conflict
    LocalVar -> conflict

unifyTelescope :: LevelId -> VTelescope -> VTelescope -> UnifyMonad SolveResult
unifyTelescope lvl lhs rhs = case (lhs, rhs) of
  (VTNil, VTNil) -> return mempty
  (VTCons lh lRstHOAS, VTCons rh rRstHOAS) -> do
    ch <- unifyTm lvl lh rh
    let pushLvl = pushEnv (vvar lvl)
    lRst <- liftEval $ evalHOAS lRstHOAS pushLvl
    rRst <- liftEval $ evalHOAS rRstHOAS pushLvl
    cr <- unifyTelescope (incrLvl lvl) lRst rRst
    return $ ch <> cr
  _ -> conflict

unifySpine :: LevelId -> Spine -> Spine -> UnifyMonad SolveResult
unifySpine lvl lhs rhs = case (lhs, rhs) of
  (SNil, SNil) -> return mempty
  (SApp lsp l, SApp rsp r) -> do
    cs <- unifySpine lvl lsp rsp
    c <- unifyTm lvl l r
    return $ cs <> c
  (SFirst lsp, SFirst rsp) -> unifySpine lvl lsp rsp
  (SRest lsp, SRest rsp) -> unifySpine lvl lsp rsp
  (SJ lFam lp lsp, SJ rFam rp rsp) -> do
    cFam <- unifyTm (shift 2 lvl) lFam rFam
    cp <- unifyTm lvl lp rp
    cs <- unifySpine lvl lsp rsp
    return $ cFam <> cp <> cs
  _ -> conflict

unifyTm :: LevelId -> Value -> Value -> UnifyMonad SolveResult
unifyTm lvl lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (VPi lDom lHOAS, VPi rDom rHOAS) -> do
      domRes <- unifyTm lvl lDom rDom
      let pushLvlVar = pushEnv $ vvar lvl
      lCod <- liftEval $ evalHOAS lHOAS pushLvlVar
      rCod <- liftEval $ evalHOAS rHOAS pushLvlVar
      codRes <- unifyTm (incrLvl lvl) lCod rCod
      return $ domRes <> codRes
    (VRecord lTele, VRecord rTele) -> unifyTelescope lvl lTele rTele
    (VId lTy lLhs lRhs, VId rTy rLhs rRhs) -> do
      tyRes <- unifyTm lvl lTy rTy
      lhsRes <- unifyTm lvl lLhs rLhs
      rhsRes <- unifyTm lvl lRhs rRhs
      return $ tyRes <> lhsRes <> rhsRes
    (VType i, VType j) -> if i == j then return mempty else conflict
    (VLam _, _) -> unifyLam lhs' rhs'
    (_, VLam _) -> unifyLam lhs' rhs'
    (VList r, _) -> unifyList r lhs' rhs'
    (_, VList r) -> unifyList r lhs' rhs'
    (Neutral (NSplicing l) lSpine, Neutral (NSplicing r) rSpine) ->
      if l == r then
        unifySpine lvl lSpine rSpine
      else
        conflict
    (Neutral (NVar l) lSpine, Neutral (NVar r) rSpine) -> do
      lEntry <- findEntry l
      rEntry <- findEntry r
      case (lEntry, rEntry) of
        (EnvVar, EnvVar) -> rigidRigid l lSpine r rSpine
        (MetaVar _, MetaVar _) -> flexFlex l lSpine r rSpine lhs' rhs'
        (ConstraintProof _, ConstraintProof _) -> flexFlex l lSpine r rSpine lhs' rhs'
        (LocalVar, LocalVar) -> rigidRigid l lSpine r rSpine
        (ConstraintProof _, MetaVar _) -> postponeEquation [l, r] lhs' rhs'
        (ConstraintProof _, _) -> postponeEquation [l] lhs' rhs'
        (MetaVar _, ConstraintProof _) -> postponeEquation [l, r] lhs' rhs'
        (_, ConstraintProof _) -> postponeEquation [r] lhs' rhs'
        (MetaVar _, _) -> solveEntry lvl l lSpine rhs'
        (_, MetaVar _) -> solveEntry lvl r rSpine lhs'
        _ -> conflict
    (Neutral (NVar l) lSpine, _) -> solveEntry lvl l lSpine rhs'
    (_, Neutral (NVar r) rSpine) -> solveEntry lvl r rSpine lhs'
    _ -> conflict
  where
    postponeEquation stuck lhs' rhs' =
      return $
        postpone $
          Equation
            { eLvl = lvl,
              eLhs = lhs',
              eRhs = rhs',
              stuckOn = Set.fromList stuck
            }
    unifyLam lhs' rhs' = do
      let lvlVar = vvar lvl
      lBody <- liftEval $ evaluateApp lhs' lvlVar
      rBody <- liftEval $ evaluateApp rhs' lvlVar
      unifyTm (incrLvl lvl) lBody rBody

    unifyList r lhs' rhs' = case size r of
      0 -> return mempty
      1 -> do
        lFst <- liftEval $ evaluateFirst lhs'
        rFst <- liftEval $ evaluateFirst rhs'

        unifyTm lvl lFst rFst
      _ -> do
        lFst <- liftEval $ evaluateFirst lhs'
        rFst <- liftEval $ evaluateFirst rhs'
        fstRes <- unifyTm lvl lFst rFst

        lRst <- liftEval $ evaluateRest lhs'
        rRst <- liftEval $ evaluateRest rhs'
        rstRes <- unifyTm lvl lRst rRst

        return $ fstRes <> rstRes

    flexFlex l lSpine r rSpine lhs' rhs' =
      -- Same metavariable on both sides.
      if l == r then do
        c <- isConsistent $ conv lvl lSpine rSpine
        -- If the argument spines are definitionally equal,
        -- this constraint is already solved
        if c then
          return mempty
        -- otherwise it stuck on l
        else
          postponeEquation [l] lhs' rhs'
      -- Different metavariables: postpone.
      else
        postponeEquation [l, r] lhs' rhs'

    rigidRigid l lSpine r rSpine =
      if l == r then
        unifySpine lvl lSpine rSpine
      else
        conflict
