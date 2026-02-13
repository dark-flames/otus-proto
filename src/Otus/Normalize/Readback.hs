module Otus.Normalize.Readback (
  readbackMeta,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Eval
import Otus.Normalize.Value

readbackMetaClosure :: LevelId -> MetaClosure -> EvalResult MetaTerm
readbackMetaClosure lvl cls = do
  b <- evaluateMetaClosure (mvvar lvl) cls
  readbackMeta (incrLvl lvl) b

readbackMetaSpine :: LevelId -> MetaTerm -> MetaSpine -> EvalResult MetaTerm
readbackMetaSpine lvl stuck = \case
  MSNil -> return stuck
  MSApp s p -> do
    sTm <- readbackMetaSpine lvl stuck s
    pTm <- readbackMeta lvl p
    return $ MApp sTm pTm
  MSForce s -> MForce <$> readbackMetaSpine lvl stuck s
  MSBind prev bCls tCls -> do
    prevTm <- readbackMetaSpine lvl stuck prev
    bTm <- readbackMetaClosure lvl bCls
    tTm <- readbackMetaClosure lvl tCls
    return $ MLetIn prevTm bTm tTm
  _ -> throwError $ Anyhow "unimplement"

readbackMeta :: LevelId -> MetaValue -> EvalResult MetaTerm
readbackMeta lvl = \case
  MNeutral h s -> readbackMetaSpine lvl (MVar $ toIndex lvl h) s
  MVPi dom e cls -> do
    domTm <- readbackMeta lvl dom
    codTm <- readbackMetaClosure lvl cls
    return $ MPi domTm e codTm
  MVLam cls -> MLam <$> readbackMetaClosure lvl cls
  MVF vty -> MF <$> readbackMeta lvl vty
  MVReturn v -> MReturn <$> readbackMeta lvl v
  MVTrigger e v -> MTrigger e <$> readbackMeta lvl v
  MVCType l -> return $ MCType l
  MVU e cty -> MU e <$> readbackMeta lvl cty
  MVThunk c -> MThunk <$> readbackMeta lvl c
  MVVType l -> return $ MVType l
  _ -> throwError $ Anyhow "unimplement"
