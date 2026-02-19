module Otus.Normalize.Unify.Unify (
  unifyTm,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Error
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Unify.Conv
import Otus.Normalize.Unify.State
import Otus.Normalize.Value

-- Problem Set
data UConstraint = UConstraint
  { cLvl :: LevelId,
    cLhs :: Value,
    cRhs :: Value
  }

type ProblemSeg = Seq UConstraint

data ProblemSet = PSet
  { pBaseLvl :: LevelId,
    segs :: ProblemSeg
  }

-- Unification
solveMetaEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad ProblemSeg
solveMetaEntry = undefined

solveEntry :: LevelId -> LevelId -> Spine -> Value -> UnifyMonad ProblemSeg
solveEntry lvl i spine val =
  findEntry i >>= \case
    EnvVar -> conflict
    MetaVar Nothing -> solveMetaEntry lvl i spine val
    MetaVar _ -> throwError CannotSoveMetaTwice
    ConstraintVar _ -> conflict
    LocalVar -> conflict

unifySpine :: LevelId -> Spine -> Spine -> UnifyMonad ProblemSeg
unifySpine lvl lhs rhs = case (lhs, rhs) of
  (SNil, SNil) -> return Empty
  (SApp lsp l, SApp rsp r) -> do
    cs <- unifySpine lvl lsp rsp
    c <- unifyTm lvl l r
    return $ cs >< c
  (SFirst lsp, SFirst rsp) -> unifySpine lvl lsp rsp
  (SRest lsp, SRest rsp) -> unifySpine lvl lsp rsp
  (SJ lFam lp lsp, SJ rFam rp rsp) -> do
    cFam <- unifyTm (shift 2 lvl) lFam rFam
    cp <- unifyTm lvl lp rp
    cs <- unifySpine lvl lsp rsp
    return $ cFam >< cp >< cs
  _ -> conflict

unifyTm :: LevelId -> Value -> Value -> UnifyMonad ProblemSeg
unifyTm lvl lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  let postponeConstraint = return $ singleton (UConstraint lvl lhs' rhs')
  let flexFlex l lSpine r rSpine =
        -- Same metavariable on both sides.
        if l == r then do
          c <- isConsistent $ conv lvl lSpine rSpine
          -- If the argument spines are definitionally equal,
          -- this constraint is already solved
          if c then
            return Empty
          -- otherwise we must postpone it
          else
            postponeConstraint
        -- Different metavariables: postpone.
        else
          postponeConstraint
  let rigidRigid l lSpine r rSpine =
        if l == r then
          unifySpine lvl lSpine rSpine
        else
          conflict
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
        conflict
    (Neutral (NVar l) lSpine, Neutral (NVar r) rSpine) -> do
      lEntry <- findEntry l
      rEntry <- findEntry r
      case (lEntry, rEntry) of
        (EnvVar, EnvVar) -> rigidRigid l lSpine r rSpine
        (MetaVar _, MetaVar _) -> flexFlex l lSpine r rSpine
        (ConstraintVar _, ConstraintVar _) -> flexFlex l lSpine r rSpine
        (LocalVar, LocalVar) -> rigidRigid l lSpine r rSpine
        (_, ConstraintVar _) -> postponeConstraint
        (ConstraintVar _, _) -> postponeConstraint
        (MetaVar _, _) -> solveEntry lvl l lSpine rhs'
        (_, MetaVar _) -> solveEntry lvl r rSpine lhs'
        _ -> conflict
    (Neutral (NVar l) lSpine, _) -> solveEntry lvl l lSpine rhs'
    (_, Neutral (NVar r) rSpine) -> solveEntry lvl r rSpine lhs'
    _ -> conflict
