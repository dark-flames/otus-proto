module Otus.Normalize.Readback (
  readbackTelescope,
  readback,
  readbackMeta,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Eval
import Otus.Normalize.Value

readbackClosure :: LevelId -> ObjClosure -> EvalResult Term
readbackClosure lvl cls = do
  b <- evaluateClosure (vvar lvl) cls
  readback (incrLvl lvl) b

readbackMetaClosure :: LevelId -> MetaClosure -> EvalResult MetaTerm
readbackMetaClosure lvl cls = do
  b <- evaluateMetaClosure (mvvar lvl) cls
  readbackMeta (incrLvl lvl) b

readbackTelescope :: LevelId -> VTelescope -> EvalResult Telescope
readbackTelescope _ VTNil = return Empty
readbackTelescope lvl (VTCons ty cls) = do
  tyTm <- readback lvl ty
  rst <- evaluateTeleClosureFresh cls
  rTm <- readbackTelescope (incrLvl lvl) rst
  return $ tyTm :<| rTm

readbackRecord :: LevelId -> VRecord -> EvalResult Record
readbackRecord lvl = mapM (readback lvl)

readbackSpine :: LevelId -> Term -> Spine -> EvalResult Term
readbackSpine lvl stuck = \case
  SNil -> return stuck
  SApp s p -> do
    sTm <- readbackSpine lvl stuck s
    pTm <- readback lvl p
    return $ App sTm pTm
  SFirst s -> First <$> readbackSpine lvl stuck s
  SRest s -> Rest <$> readbackSpine lvl stuck s

readbackMetaSpine :: LevelId -> MetaTerm -> MetaSpine -> EvalResult MetaTerm
readbackMetaSpine lvl stuck = \case
  MSNil -> return stuck
  MSApp s p -> do
    sTm <- readbackMetaSpine lvl stuck s
    pTm <- readbackMeta lvl p
    return $ MApp sTm pTm
  MSForce s -> MForce <$> readbackMetaSpine lvl stuck s
  MSBind prev bCls tyCls -> do
    prevTm <- readbackMetaSpine lvl stuck prev
    bTm <- readbackMetaClosure lvl bCls
    bindTyTm <- readbackMetaClosure lvl tyCls
    return $ MLetIn prevTm bTm bindTyTm
  _ -> throwError $ Anyhow "unimplement"

readbackMeta :: LevelId -> MetaValue -> EvalResult MetaTerm
readbackMeta lvl = \case
  MNeutral h s -> readbackMetaSpine lvl (MVar $ toIndex lvl h) s
  MVPi dom e cls -> do
    domTm <- readbackMeta lvl dom
    codTm <- readbackMetaClosure lvl cls
    return $ MPi domTm e codTm
  MVLam cls -> MLam Nothing <$> readbackMetaClosure lvl cls
  MVF vty -> MF <$> readbackMeta lvl vty
  MVReturn v -> MReturn <$> readbackMeta lvl v
  MVTrigger e -> return $ MTrigger e
  MVCType l -> return $ MCType l
  MVU e cty -> MU e <$> readbackMeta lvl cty
  MVThunk c -> MThunk <$> readbackMeta lvl c
  MVVType l -> return $ MVType l
  _ -> throwError $ Anyhow "unimplement"

readback :: LevelId -> Value -> EvalResult Term
readback lvl = \case
  Neutral stuck spine -> do
    h <- case stuck of
      NVar l -> return $ Var $ toIndex lvl l
      NSplicing l -> return $ Splicing $ MVar (toIndex lvl l)
    readbackSpine lvl h spine
  VPi dom cls -> do
    domTm <- readback lvl dom
    codTm <- readbackClosure lvl cls
    return $ Pi domTm codTm
  VLam cls -> Lam Nothing <$> readbackClosure lvl cls
  VRecord tele -> Record <$> readbackTelescope lvl tele
  VList list -> List <$> readbackRecord lvl list
  VType l -> return $ Type l
