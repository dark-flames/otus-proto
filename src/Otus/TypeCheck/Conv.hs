module Otus.TypeCheck.Conv (
  ConvCheck (..),
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Error

class ConvCheck val where
  conv :: LevelId -> val -> val -> TypeCheckResult Bool

instance ConvCheck VTeleSequence where
  conv lvl lhs rhs = go lvl (unVTele lhs) (unVTele rhs)
    where
      go _ Empty Empty = return True
      go l (lh :<| lRst) (rh :<| rRst) = do
        hConv <- conv l lh rh
        if hConv then do
          go (incrLvl l) lRst rRst
        else
          return False
      go _ _ _ = return False

instance ConvCheck VTelescope where
  conv lvl lhs rhs = case (lhs, rhs) of
    (VTNil, VTNil) -> return True
    (VTCons lh lRstHOAS, VTCons rh rRstHOAS) -> do
      hConv <- conv lvl lh rh
      if hConv then do
        let pushLvl = pushEnv (vvar lvl)
        lRst <- doEvalHOAS pushLvl lRstHOAS
        rRst <- doEvalHOAS pushLvl rRstHOAS
        conv (incrLvl lvl) lRst rRst
      else
        return False
    _ -> return False

instance ConvCheck VRecord where
  conv _ Empty Empty = return True
  conv lvl (lh :<| lr) (rh :<| rr) = do
    hConv <- conv lvl lh rh
    if hConv then
      conv lvl lr rr
    else
      return False
  conv _ _ _ = return False

instance ConvCheck VConstraint where
  conv lvl (VTmEq lLift lLhs lRhs lTy) (VTmEq rLift rLhs rRhs rTy) =
    if lLift == rLift then do
      let l = shift lLift lvl
      tyConv <- conv l lTy rTy
      if tyConv then do
        lhsConv <- conv l lLhs rLhs
        rhsConv <- conv l lRhs rRhs
        return $ lhsConv && rhsConv
      else do
        return False
    else
      return False

instance ConvCheck VProblem where
  conv _ Empty Empty = return True
  conv lvl (lp :<| lr) (rp :<| rr) = do
    hConv <- conv lvl lp rp
    if hConv then
      conv lvl lr rr
    else
      return False
  conv _ _ _ = return False

instance ConvCheck Spine where
  conv lvl lhs rhs = case (lhs, rhs) of
    (SNil, SNil) -> return True
    (SApp lh lp, SApp rh rp) -> do
      pConv <- conv lvl lp rp
      if pConv then
        conv lvl lh rh
      else
        return False
    (SFirst lh, SFirst rh) -> conv lvl lh rh
    (SRest lh, SRest rh) -> conv lvl lh rh
    _ -> return False

instance ConvCheck MetaSpine where
  conv lvl lhs rhs = case (lhs, rhs) of
    (MSNil, MSNil) -> return True
    (MSApp lh lp, MSApp rh rp) -> do
      pConv <- conv lvl lp rp
      if pConv then
        conv lvl lh rh
      else
        return False
    (MSForce lh, MSForce rh) -> conv lvl lh rh
    (MSBind lh lHOAS _, MSBind rh rHOAS _) -> do
      let p = mvvar lvl
      lbind <- doEvalHOAS (pushEnv p) lHOAS
      rbind <- doEvalHOAS (pushEnv p) rHOAS
      bindConv <- conv (incrLvl lvl) lbind rbind
      if bindConv then
        conv lvl lh rh
      else
        return False
    _ -> return False

instance ConvCheck Value where
  conv lvl lhs rhs = case (lhs, rhs) of
    (Neutral lh ls, Neutral rh rs) -> case (lh, rh) of
      (NVar l, NVar r) -> if l == r then conv lvl ls rs else return False
      (NSplicing l, NSplicing r) -> if l == r then conv lvl ls rs else return False
      _ -> return False
    (VPi lDom lHOAS, VPi rDom rHOAS) -> do
      domConv <- conv lvl lDom rDom
      if domConv then do
        let pushLvl = pushEnv (vvar lvl)
        lCod <- doEvalHOAS pushLvl lHOAS
        rCod <- doEvalHOAS pushLvl rHOAS
        conv (incrLvl lvl) lCod rCod
      else
        return False
    (VLam _, _) -> do
      let p = vvar lvl
      lRes <- doEvalApp lvl lhs p
      rRes <- doEvalApp lvl rhs p
      conv (incrLvl lvl) lRes rRes
    (_, VLam _) -> do
      let p = vvar lvl
      lRes <- doEvalApp lvl lhs p
      rRes <- doEvalApp lvl rhs p
      conv (incrLvl lvl) lRes rRes
    (VRecord lTele, VRecord rTele) -> conv lvl lTele rTele
    (VList lr, VList rr) -> conv lvl lr rr
    _ -> return False

instance ConvCheck MetaValue where
  conv lvl lhs rhs = case (lhs, rhs) of
    (MVU le lc, MVU re rc) ->
      if le `lte` re == Just True then
        conv lvl lc rc
      else
        return False
    (MVThunk lc, MVThunk rc) -> conv lvl lc rc
    (MVVType l, MVVType l') -> return $ l <= l'
    (MVLift lTele, MVLift rTele) -> conv lvl lTele rTele
    (MVQuote lList, MVQuote rList) -> conv lvl lList rList
    (MVDyn lTele, MVDyn rTele) -> conv lvl lTele rTele
    (MVGuard lMeta lProblem lRecordHOAS, MVGuard rMeta rProblem rRecordHOAS) -> do
      let metaSize = size lMeta
      let problemSize = size lProblem
      metaConv <- conv lvl lMeta rMeta
      problemConv <- conv (shift metaSize lvl) lProblem rProblem
      let pushLvls = liftObjEnvN (metaSize + problemSize)
      lRecord <- doEvalHOAS pushLvls lRecordHOAS
      rRecord <- doEvalHOAS pushLvls rRecordHOAS
      recordConv <- conv (shift (metaSize + problemSize) lvl) lRecord rRecord
      return $ metaConv && problemConv && recordConv
    -- computation
    (MVPi lDom le lHOAS, MVPi rDom re rHOAS) -> do
      domConv <- conv lvl lDom rDom
      let eConv = le `lte` re == Just True
      if domConv && eConv then do
        let pushLvl = pushEnv (mvvar lvl)
        lCod <- doEvalHOAS pushLvl lHOAS
        rCod <- doEvalHOAS pushLvl rHOAS
        conv (incrLvl lvl) lCod rCod
      else
        return False
    (MVLam _, _) -> do
      let p = mvvar lvl
      lRes <- doEvalMApp lvl lhs p
      rRes <- doEvalMApp lvl rhs p
      conv (incrLvl lvl) lRes rRes
    (_, MVLam _) -> do
      let p = mvvar lvl
      lRes <- doEvalMApp lvl lhs p
      rRes <- doEvalMApp lvl rhs p
      conv (incrLvl lvl) lRes rRes
    (MVF lv, MVF rv) -> conv lvl lv rv
    (MVReturn lv, MVReturn rv) -> conv lvl lv rv
    (MVTrigger le, MVTrigger re) -> return $ le == re
    (MVCType l, MVCType l') -> return $ l <= l'
    (MNeutral lh ls, MNeutral rh rs) ->
      if lh == rh then
        conv lvl ls rs
      else
        return False
    _ -> return False
