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
readbackNeutral lvl = \case
  ONVar vLvl -> return $ OVar $ toIndex lvl vLvl
  ONApp vh spine -> do
    h <- readbackNeutral lvl vh
    seqFoldlM (\h' vArg -> OApp h' <$> readback lvl vArg) h spine

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
  OVType s u -> return $ OType s u
  OVNeutral neu -> readbackNeutral lvl neu

normalize :: LevelId -> ObjTerm -> ObjEvalResult ObjTerm
normalize lvl tm = do
  let env = normalizeEnv lvl
  val <- evaluateObj tm env
  readback lvl val
