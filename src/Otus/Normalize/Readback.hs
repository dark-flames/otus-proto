module Otus.Normalize.Readback (
  Quotable (..),
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Eval
import Otus.Normalize.Value

class (Show v) => Quotable v where
  type QuoteRes v
  quote :: LevelId -> v -> EvalResult (QuoteRes v)

-- Object Language ReadBack
instance Quotable ObjClosure where
  type QuoteRes ObjClosure = Term
  quote lvl cls = do
    b <- evaluateClosure (vvar lvl) cls
    quote (incrLvl lvl) b

instance Quotable VTelescope where
  type QuoteRes VTelescope = Telescope
  quote _ VTNil = return $ TeleSeq Empty
  quote lvl (VTCons ty cls) = do
    tyTm <- quote lvl ty
    rst <- evaluateClosure (vvar lvl) cls
    rTm <- quote (incrLvl lvl) rst
    return $ TeleSeq (tyTm :<| unTele rTm)

instance Quotable VRecord where
  type QuoteRes VRecord = Record

  quote lvl r = RecordSeq <$> mapM (quote lvl) r

quoteSpine :: LevelId -> Term -> Spine -> EvalResult Term
quoteSpine lvl stuck = \case
  SNil -> return stuck
  SApp s p -> do
    sTm <- quoteSpine lvl stuck s
    pTm <- quote lvl p
    return $ App sTm pTm
  SFirst s -> First <$> quoteSpine lvl stuck s
  SRest s -> Rest <$> quoteSpine lvl stuck s

instance Quotable Value where
  type QuoteRes Value = Term
  quote lvl = \case
    Neutral stuck spine -> do
      h <- case stuck of
        NVar l -> return $ Var $ toIndex lvl l
        NSplicing l -> return $ Splicing $ MVar (toIndex lvl l)
      quoteSpine lvl h spine
    VPi dom cls -> do
      domTm <- quote lvl dom
      codTm <- quote lvl cls
      return $ Pi domTm codTm
    VLam cls -> Lam Nothing <$> quote lvl cls
    VRecord tele -> Record <$> quote lvl tele
    VList list -> List <$> quote lvl list
    VType l -> return $ Type l

-- Meta Language Readback
instance Quotable MetaClosure where
  type QuoteRes MetaClosure = MetaTerm
  quote lvl cls = do
    b <- evaluateClosure (mvvar lvl) cls
    quote (incrLvl lvl) b

quoteMSpine :: LevelId -> MetaTerm -> MetaSpine -> EvalResult MetaTerm
quoteMSpine lvl stuck = \case
  MSNil -> return stuck
  MSApp s p -> do
    sTm <- quoteMSpine lvl stuck s
    pTm <- quote lvl p
    return $ MApp sTm pTm
  MSForce s -> MForce <$> quoteMSpine lvl stuck s
  MSBind prev bCls tyCls -> do
    prevTm <- quoteMSpine lvl stuck prev
    bTm <- quote lvl bCls
    bindTyTm <- quote lvl tyCls
    return $ MLetIn prevTm bTm bindTyTm
  _ -> throwError $ Anyhow "unimplement"

instance Quotable MetaValue where
  type QuoteRes MetaValue = MetaTerm
  quote lvl = \case
    MNeutral h s -> quoteMSpine lvl (MVar $ toIndex lvl h) s
    MVPi dom e cls -> do
      domTm <- quote lvl dom
      codTm <- quote lvl cls
      return $ MPi domTm e codTm
    MVLam cls -> MLam Nothing <$> quote lvl cls
    MVF vty -> MF <$> quote lvl vty
    MVReturn v -> MReturn <$> quote lvl v
    MVTrigger e -> return $ MTrigger e
    MVCType l -> return $ MCType l
    MVU e cty -> MU e <$> quote lvl cty
    MVThunk c -> MThunk <$> quote lvl c
    MVVType l -> return $ MVType l
    _ -> throwError $ Anyhow "unimplement"
