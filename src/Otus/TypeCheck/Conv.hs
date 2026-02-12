module Otus.TypeCheck.Conv (
  valueConv,
  computationConv,
) where

import Otus.Ast
import Otus.Normalize
import Otus.TypeCheck.Error

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
    lbind <- mapEvalResult $ evaluateMetaClosure p lCls
    rbind <- mapEvalResult $ evaluateMetaClosure p rCls
    bindConv <- valueConv (incrLvl lvl) lbind rbind
    if bindConv then
      metaSpineConv lvl lh rh
    else
      return False
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
      lCod <- mapEvalResult $ evaluateMetaClosure (mvvar lvl) lCls
      rCod <- mapEvalResult $ evaluateMetaClosure (mvvar lvl) rCls
      computationConv (incrLvl lvl) lCod rCod
    else
      return False
  (MVLam _, _) -> do
    let p = mvvar lvl
    lRes <- mapEvalResult $ evaluateMApp lhs p
    rRes <- mapEvalResult $ evaluateMApp rhs p
    computationConv (incrLvl lvl) lRes rRes
  (_, MVLam _) -> do
    let p = mvvar lvl
    lRes <- mapEvalResult $ evaluateMApp lhs p
    rRes <- mapEvalResult $ evaluateMApp rhs p
    computationConv (incrLvl lvl) lRes rRes
  (MVF lv, MVF rv) -> valueConv lvl lv rv
  (MVReturn lv, MVReturn rv) -> valueConv lvl lv rv
  (MVTrigger le _, MVTrigger re _) -> return $ le == re
  (MVCType l, MVCType l') -> return $ l <= l'
  (MNeutral lh ls, MNeutral rh rs) ->
    if lh == rh then
      metaSpineConv lvl ls rs
    else
      return False
  _ -> return False
