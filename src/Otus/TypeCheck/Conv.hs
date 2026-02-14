module Otus.TypeCheck.Conv (
  conv,
  valueConv,
  computationConv,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Error

teleConv :: LevelId -> VTelescope -> VTelescope -> TypeCheckResult Bool
teleConv _ VTNil VTNil = return True
teleConv lvl (VTCons lh lCls) (VTCons rh rCls) = do
  hConv <- conv lvl lh rh
  if hConv then do
    let p = vvar lvl
    lRst <- doEvalTeleClosure p lCls
    rRst <- doEvalTeleClosure p rCls
    teleConv (incrLvl lvl) lRst rRst
  else
    return False
teleConv _ _ _ = return False

recordConv :: LevelId -> VRecord -> VRecord -> TypeCheckResult Bool
recordConv _ Empty Empty = return True
recordConv lvl (lh :<| lr) (rh :<| rr) = do
  hConv <- conv lvl lh rh
  if hConv then
    recordConv lvl lr rr
  else
    return False
recordConv _ _ _ = return False

spineConv :: LevelId -> Spine -> Spine -> TypeCheckResult Bool
spineConv lvl lhs rhs = case (lhs, rhs) of
  (SNil, SNil) -> return True
  (SApp lh lp, SApp rh rp) -> do
    pConv <- conv lvl lp rp
    if pConv then
      spineConv lvl lh rh
    else
      return False
  (SFirst lh, SFirst rh) -> spineConv lvl lh rh
  (SRest lh, SRest rh) -> spineConv lvl lh rh
  _ -> return False

metaSpineConv :: LevelId -> MetaSpine -> MetaSpine -> TypeCheckResult Bool
metaSpineConv lvl lhs rhs = case (lhs, rhs) of
  (MSNil, MSNil) -> return True
  (MSApp lh lp, MSApp rh rp) -> do
    pConv <- valueConv lvl lp rp
    if pConv then
      metaSpineConv lvl lh rh
    else
      return False
  (MSForce lh, MSForce rh) -> metaSpineConv lvl lh rh
  (MSBind lh lCls _, MSBind rh rCls _) -> do
    let p = mvvar lvl
    lbind <- doEvalMetaClosure p lCls
    rbind <- doEvalMetaClosure p rCls
    bindConv <- valueConv (incrLvl lvl) lbind rbind
    if bindConv then
      metaSpineConv lvl lh rh
    else
      return False
  _ -> return False

conv :: LevelId -> Value -> Value -> TypeCheckResult Bool
conv lvl lhs rhs = case (lhs, rhs) of
  (Neutral lh ls, Neutral rh rs) -> case (lh, rh) of
    (NVar l, NVar r) -> if l == r then spineConv lvl ls rs else return False
    (NSplicing l, NSplicing r) -> if l == r then spineConv lvl ls rs else return False
    _ -> return False
  (VPi lDom lCls, VPi rDom rCls) -> do
    domConv <- conv lvl lDom rDom
    if domConv then do
      lCod <- doEvalClosure (vvar lvl) lCls
      rCod <- doEvalClosure (vvar lvl) rCls
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
  (VRecord lTele, VRecord rTele) -> teleConv lvl lTele rTele
  (VList lr, VList rr) -> recordConv lvl lr rr
  _ -> return False

valueConv :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult Bool
valueConv lvl lhs rhs = case (lhs, rhs) of
  (MVU le lc, MVU re rc) ->
    if le `lte` re == Just True then
      computationConv lvl lc rc
    else
      return False
  (MVThunk lc, MVThunk rc) -> computationConv lvl lc rc
  (MVVType l, MVVType l') -> return $ l <= l'
  (MNeutral lh MSNil, MNeutral rh MSNil) -> return $ lh == rh -- only variable can be neutral value
  _ -> return False

computationConv :: LevelId -> MetaValue -> MetaValue -> TypeCheckResult Bool
computationConv lvl lhs rhs = case (lhs, rhs) of
  (MVPi lDom le lCls, MVPi rDom re rCls) -> do
    domConv <- valueConv lvl lDom rDom
    let eConv = le `lte` re == Just True
    if domConv && eConv then do
      lCod <- doEvalMetaClosure (mvvar lvl) lCls
      rCod <- doEvalMetaClosure (mvvar lvl) rCls
      computationConv (incrLvl lvl) lCod rCod
    else
      return False
  (MVLam _, _) -> do
    let p = mvvar lvl
    lRes <- doEvalMApp lvl lhs p
    rRes <- doEvalMApp lvl rhs p
    computationConv (incrLvl lvl) lRes rRes
  (_, MVLam _) -> do
    let p = mvvar lvl
    lRes <- doEvalMApp lvl lhs p
    rRes <- doEvalMApp lvl rhs p
    computationConv (incrLvl lvl) lRes rRes
  (MVF lv, MVF rv) -> valueConv lvl lv rv
  (MVReturn lv, MVReturn rv) -> valueConv lvl lv rv
  (MVTrigger le, MVTrigger re) -> return $ le == re
  (MVCType l, MVCType l') -> return $ l <= l'
  (MNeutral lh ls, MNeutral rh rs) ->
    if lh == rh then
      metaSpineConv lvl ls rs
    else
      return False
  _ -> return False
