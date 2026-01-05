module Otus.Normalize.Object.Quote (
  readback,
  normalize,
) where

import Otus.Ast
import Otus.Common
import Otus.Normalize.Env
import Otus.Normalize.Object.Error
import Otus.Normalize.Object.Eval
import Otus.Normalize.Object.Value

readbackNeutral :: LevelId -> ObjNeutral -> ObjEvalResult ObjTerm
readbackNeutral ctx = \case
  ONRigid lvl spine -> go (OVar $ toIndex ctx lvl) spine
  ONFlex m spine -> go (OMeta m) spine
  where
    go :: ObjTerm -> Seq ObjValue -> ObjEvalResult ObjTerm
    go = seqFoldlM (\h vArg -> OApp h <$> readback ctx vArg)

readback :: LevelId -> ObjValue -> ObjEvalResult ObjTerm
readback lvl = \case
  OVPi vDom codCls -> do
    dom <- readback lvl vDom
    (vCod, _) <- evalClosureFresh codCls
    cod <- readback (1 + lvl) vCod
    return $ OPi dom cod
  OVLam bodyCls -> do
    (vBody, _) <- evalClosureFresh bodyCls
    body <- readback (1 + lvl) vBody
    return $ OLam body
  OVType -> return OType
  OVNeutral neu -> readbackNeutral lvl neu

normalize :: LevelId -> ObjTerm -> ObjEvalResult ObjTerm
normalize lvl tm = do
  let env = pushFreshVarN' (unLevel lvl) eempty
  val <- evaluateObj tm env
  readback lvl val
