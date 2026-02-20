module Otus.Normalize.Unify.Unify (
  solveProblem,
  unifyTm,
) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))

import qualified Data.IntMap as IM
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
data PartialRenaming = PRen
  { prDom :: LevelId,
    prCod :: LevelId,
    prRen :: IM.IntMap LevelId
  }
  deriving (Eq, Show)

liftPRenaming :: PartialRenaming -> PartialRenaming
liftPRenaming (PRen dom cod ren) =
  PRen (dom + 1) (cod + 1) (IM.insert (unLevel cod) dom ren)

invert :: LevelId -> Spine -> UnifyMonad (Maybe (PartialRenaming, Int))
invert cod sp =
  go sp >>= \case
    Just (dom, ren, s) -> return $ Just (PRen dom cod ren, s)
    _ -> return Nothing
  where
    go :: Spine -> UnifyMonad (Maybe (LevelId, IM.IntMap LevelId, Int))
    go = \case
      SNil -> return $ Just (0, mempty, 0)
      SApp sp' param ->
        go sp' >>= \case
          Just (dom, ren, s) -> do
            param' <- force param
            case param' of
              Neutral (NVar l@(LevelId x)) SNil ->
                findEntry l >>= \case
                  LocalVar ->
                    if IM.notMember x ren then
                      return $ Just (dom + 1, IM.insert x dom ren, s + 1)
                    else
                      return Nothing
                  _ -> return Nothing
              _ -> return Nothing
          _ -> return Nothing
      _ -> return Nothing

rename :: LevelId -> PartialRenaming -> Value -> UnifyMonad (Maybe Term)
rename m = go
  where
    goSpine :: PartialRenaming -> Term -> Spine -> UnifyMonad (Maybe Term)
    goSpine pren h = \case
      SNil -> return $ Just h
      SApp s a -> do
        ma <- go pren a
        case ma of
          Just a' -> goSpine pren (App h a') s
          Nothing -> return Nothing
      SFirst s -> goSpine pren (First h) s
      SRest s -> goSpine pren (Rest h) s
      SJ fam p s -> do
        mfam <- go pren fam
        mp <- go pren p
        case (mfam, mp) of
          (Just fam', Just p') -> goSpine pren (J fam' p' h) s
          _ -> return Nothing

    goTelescope :: PartialRenaming -> VTelescope -> UnifyMonad (Maybe Telescope)
    goTelescope pren = \case
      VTNil -> return $ Just (TeleSeq Empty)
      VTCons ty hoas -> do
        mTyTm <- go pren ty
        rst <- liftEval $ evalHOAS hoas (pushEnv $ vvar (prCod pren))
        mRst <- goTelescope (liftPRenaming pren) rst
        return $ do
          tyTm <- mTyTm
          TeleSeq rstSeq <- mRst
          return $ TeleSeq (tyTm <| rstSeq)

    go :: PartialRenaming -> Value -> UnifyMonad (Maybe Term)
    go pren t =
      force t
        >>= \case
          Neutral (NSplicing l) sp -> goSpine pren (Var $ toIndex (prDom pren) l) sp
          Neutral (NVar l) sp ->
            findEntry l >>= \case
              EnvVar -> goSpine pren (Var $ toIndex (prDom pren) l) sp
              MetaVar _ ->
                if l < m then
                  goSpine pren (Var $ toIndex (prDom pren) l) sp
                else
                  return Nothing
              ConstraintProof _ -> return Nothing
              LocalVar -> case IM.lookup (unLevel l) (prRen pren) of
                Nothing -> return Nothing
                Just l' -> goSpine pren (Var $ toIndex (prDom pren) l') sp
          VLam hoas -> do
            body <- liftEval $ evalHOAS hoas (pushEnv $ vvar (prCod pren))
            mBodyTm <- go (liftPRenaming pren) body
            return $ Lam Nothing <$> mBodyTm
          VPi a hoas -> do
            mATm <- go pren a
            b <- liftEval $ evalHOAS hoas (pushEnv $ vvar (prCod pren))
            mBTm <- go (liftPRenaming pren) b
            return $ Pi <$> mATm <*> mBTm
          VRecord tele -> do
            mTele <- goTelescope pren tele
            return $ Record <$> mTele
          VList record -> do
            mRecord <- traverse (go pren) record
            return $ List . RecordSeq <$> sequenceA mRecord
          VId ty a b -> do
            mTyTm <- go pren ty
            mATm <- go pren a
            mBTm <- go pren b
            return $ Id <$> mTyTm <*> mATm <*> mBTm
          VRefl -> return $ Just Refl
          VType i -> return $ Just (Type i)

solveMetaEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad SolveResult
solveMetaEntry lvl metaId sp rhs =
  invert lvl sp >>= \case
    Just (pren, spSize) ->
      rename metaId pren rhs >>= \case
        Just rhsTm -> do
          let solutionTm = lamN spSize rhsTm
          solution <- liftEval $ evaluateTerm solutionTm (trivalEnv lvl)
          setEntry metaId (MetaVar (Just solution))
          return $ SolveResult mempty (Set.singleton metaId)
        _ -> postponeEquation
    _ -> postponeEquation
  where
    postponeEquation :: UnifyMonad SolveResult
    postponeEquation =
      return $
        SolveResult
          ( singleton
              ( Equation
                  { eLvl = lvl,
                    eLhs = Neutral (NVar metaId) sp,
                    eRhs = rhs,
                    stuckOn = Set.singleton metaId -- todo: meta in rhs
                  }
              )
          )
          mempty

solveEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad SolveResult
solveEntry lvl i spine val =
  findEntry i >>= \case
    EnvVar -> throwError CannotSolveEnvVar
    MetaVar Nothing -> solveMetaEntry lvl i spine val
    MetaVar _ -> throwError CannotSolveMetaTwice
    ConstraintProof _ -> throwError CannotSolveConstraintProof
    LocalVar -> throwError CannotSolveLocalVar

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

unifyRecord :: LevelId -> VRecord -> VRecord -> UnifyMonad SolveResult
unifyRecord lvl lhs rhs = case (lhs, rhs) of
  (Empty, Empty) -> return mempty
  (l :<| lRst, r :<| rRst) -> do
    hRes <- unifyTm lvl l r
    rstRes <- unifyRecord lvl lRst rRst
    return $ hRes <> rstRes
  _ -> conflict

unifySpine :: LevelId -> Spine -> Spine -> UnifyMonad SolveResult
unifySpine lvl lhs rhs = case (lhs, rhs) of
  (SNil, SNil) -> return mempty
  (SApp lsp l, SApp rsp r) -> do
    sRes <- unifySpine lvl lsp rsp
    pRes <- unifyTm lvl l r
    return $ sRes <> pRes
  (SFirst lsp, SFirst rsp) -> unifySpine lvl lsp rsp
  (SRest lsp, SRest rsp) -> unifySpine lvl lsp rsp
  (SJ lFam lp lsp, SJ rFam rp rsp) -> do
    famRes <- unifyTm (shift 2 lvl) lFam rFam
    pRes <- unifyTm lvl lp rp
    sRes <- unifySpine lvl lsp rsp
    return $ famRes <> pRes <> sRes
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
    (VList lr, VList rr) -> unifyRecord lvl lr rr -- todo : eta-conv for record
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
