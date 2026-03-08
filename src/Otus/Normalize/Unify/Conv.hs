{-# LANGUAGE InstanceSigs #-}

module Otus.Normalize.Unify.Conv (
  ConvCheck (..),
) where

import Otus.Ast
import Otus.Common
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Unify.State
import Otus.Normalize.Value

class ConvCheck val where
  conv :: LevelId -> val -> val -> UnifyMonad ()

instance ConvCheck VTeleSequence where
  conv lvl lhs rhs = go lvl (unVTele lhs) (unVTele rhs)
    where
      go _ Empty Empty = return ()
      go l (lh :<| lRst) (rh :<| rRst) = do
        conv l lh rh
        go (incrLvl l) lRst rRst
      go _ _ _ = conflict

instance ConvCheck VTelescope where
  conv lvl lhs rhs = case (lhs, rhs) of
    (VTNil, VTNil) -> return ()
    (VTCons lh lRstHOAS, VTCons rh rRstHOAS) -> do
      conv lvl lh rh
      let pushLvl = pushEnv (vvar lvl)
      lRst <- liftEval $ evalHOAS lRstHOAS pushLvl
      rRst <- liftEval $ evalHOAS rRstHOAS pushLvl
      conv (incrLvl lvl) lRst rRst
    _ -> conflict

instance ConvCheck VRecord where
  conv _ Empty Empty = return ()
  conv lvl (lh :<| lr) (rh :<| rr) = do
    conv lvl lh rh
    conv lvl lr rr
  conv _ _ _ = conflict

instance ConvCheck VConstraint where
  conv lvl (VTmEq lTele lLhs lRhs lTy) (VTmEq rTele rLhs rRhs rTy) = do
    conv lvl lTele rTele
    let l = shift (size lTele) lvl
    conv l lTy rTy
    conv l lLhs rLhs
    conv l lRhs rRhs
  conv lvl (VMetaDef lTy) (VMetaDef rTy) = conv lvl lTy rTy
  conv _ _ _ = conflict

instance ConvCheck VProblem where
  conv _ Empty Empty = return ()
  conv lvl (lp :<| lr) (rp :<| rr) = do
    conv lvl lp rp
    conv lvl lr rr
  conv _ _ _ = conflict

instance ConvCheck Spine where
  conv lvl lhs rhs = case (lhs, rhs) of
    (SNil, SNil) -> return ()
    (SApp lh lp, SApp rh rp) -> do
      conv lvl lp rp
      conv lvl lh rh
    (SFirst lh, SFirst rh) -> conv lvl lh rh
    (SRest lh, SRest rh) -> conv lvl lh rh
    _ -> conflict

instance ConvCheck MetaSpine where
  conv :: LevelId -> MetaSpine -> MetaSpine -> UnifyMonad ()
  conv lvl lhs rhs = case (lhs, rhs) of
    (MSNil, MSNil) -> return ()
    (MSApp lh lp, MSApp rh rp) -> do
      conv lvl lp rp
      conv lvl lh rh
    (MSForce lh, MSForce rh) -> conv lvl lh rh
    (MSBind lh lHOAS _, MSBind rh rHOAS _) -> do
      let p = mvvar lvl
      lbind <- liftEval $ evalHOAS lHOAS (pushEnv p)
      rbind <- liftEval $ evalHOAS rHOAS (pushEnv p)
      conv (incrLvl lvl) lbind rbind
      conv lvl lh rh
    _ -> conflict

instance ConvCheck Value where
  conv lvl lhs' rhs' = do
    lhs <- force lhs'
    rhs <- force rhs'
    let listConv r = case size r of
          0 -> return ()
          1 -> do
            lFst <- liftEval $ evaluateFirst lhs
            rFst <- liftEval $ evaluateFirst rhs
            conv lvl lFst rFst
          _ -> do
            lFst <- liftEval $ evaluateFirst lhs
            rFst <- liftEval $ evaluateFirst rhs
            conv lvl lFst rFst
            lRst <- liftEval $ evaluateRest lhs
            rRst <- liftEval $ evaluateRest rhs
            conv lvl lRst rRst
    case (lhs, rhs) of
      (Neutral lh ls, Neutral rh rs) -> case (lh, rh) of
        (NVar l, NVar r) -> if l == r then conv lvl ls rs else conflict
        (NSplicing l, NSplicing r) -> if l == r then conv lvl ls rs else conflict
        _ -> conflict
      (VPi lDom lHOAS, VPi rDom rHOAS) -> do
        conv lvl lDom rDom
        let pushLvl = pushEnv (vvar lvl)
        lCod <- liftEval $ evalHOAS lHOAS pushLvl
        rCod <- liftEval $ evalHOAS rHOAS pushLvl
        conv (incrLvl lvl) lCod rCod
      (VLam _, _) -> do
        let p = vvar lvl
        lRes <- liftEval $ evaluateApp lhs p
        rRes <- liftEval $ evaluateApp rhs p
        conv (incrLvl lvl) lRes rRes
      (_, VLam _) -> do
        let p = vvar lvl
        lRes <- liftEval $ evaluateApp lhs p
        rRes <- liftEval $ evaluateApp rhs p
        conv (incrLvl lvl) lRes rRes
      (VRecord lTele, VRecord rTele) -> conv lvl lTele rTele
      (VList r, _) -> listConv r
      (_, VList r) -> listConv r
      _ -> conflict

instance ConvCheck MetaValue where
  conv lvl lhs rhs = case (lhs, rhs) of
    (MVU le lc, MVU re rc) ->
      if le `lte` re == Just True then
        conv lvl lc rc
      else
        conflict
    (MVThunk lc, MVThunk rc) -> conv lvl lc rc
    (MVVType l, MVVType l') -> conflictIf $ l > l'
    (MVLift lTele, MVLift rTele) -> conv lvl lTele rTele
    (MVQuote lList, MVQuote rList) -> conv lvl lList rList
    (MVDyn lTele, MVDyn rTele) -> conv lvl lTele rTele
    (MVGuard lProblem lRecordHOAS, MVGuard rProblem rRecordHOAS) -> do
      let problemSize = size lProblem
      conv lvl lProblem rProblem
      let pushLvls = liftObjEnvN problemSize
      lRecord <- liftEval $ evalHOAS lRecordHOAS pushLvls
      rRecord <- liftEval $ evalHOAS rRecordHOAS pushLvls
      conv (shift problemSize lvl) lRecord rRecord
    -- computation
    (MVPi lDom le lHOAS, MVPi rDom re rHOAS) -> do
      conv lvl lDom rDom
      conflictIf $ le `lte` re /= Just True
      let pushLvl = pushEnv (mvvar lvl)
      lCod <- liftEval $ evalHOAS lHOAS pushLvl
      rCod <- liftEval $ evalHOAS rHOAS pushLvl
      conv (incrLvl lvl) lCod rCod
    (MVLam _, _) -> do
      let p = mvvar lvl
      lRes <- liftEval $ evaluateMApp lhs p
      rRes <- liftEval $ evaluateMApp rhs p
      conv (incrLvl lvl) lRes rRes
    (_, MVLam _) -> do
      let p = mvvar lvl
      lRes <- liftEval $ evaluateMApp lhs p
      rRes <- liftEval $ evaluateMApp rhs p
      conv (incrLvl lvl) lRes rRes
    (MVF lv, MVF rv) -> conv lvl lv rv
    (MVReturn lv, MVReturn rv) -> conv lvl lv rv
    (MVTrigger le, MVTrigger re) -> conflictIf $ le /= re
    (MVCType l, MVCType l') -> conflictIf $ l > l'
    (MNeutral lh ls, MNeutral rh rs) ->
      if lh == rh then
        conv lvl ls rs
      else
        conflict
    _ -> conflict
