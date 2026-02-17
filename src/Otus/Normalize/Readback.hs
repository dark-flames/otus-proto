module Otus.Normalize.Readback (
  Quotable (..),
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Error
import Otus.Normalize.Value

class Quotable v where
  type QuoteRes v
  quote :: LevelId -> v -> EvalResult (QuoteRes v)

-- Object Language ReadBack

instance Quotable ObjHOAS where
  type QuoteRes ObjHOAS = Term
  quote lvl hoas = do
    val <- evalHOAS hoas liftObjEnv
    quote (incrLvl lvl) val

instance Quotable VTelescope where
  type QuoteRes VTelescope = Telescope

  quote lvl t = TeleSeq <$> go lvl t
    where
      go l = \case
        VTNil -> return Empty
        VTCons ty rstHOAS -> do
          tyTm <- quote l ty
          rst <- evalHOAS rstHOAS liftObjEnv
          rstTm <- go (incrLvl l) rst
          return $ tyTm :<| rstTm

instance Quotable VTeleSequence where
  type QuoteRes VTeleSequence = Telescope

  quote lvl t = TeleSeq <$> go lvl (unVTele t)
    where
      go l = \case
        Empty -> return Empty
        ty :<| rst -> do
          tyTm <- quote l ty
          rstTm <- go (incrLvl l) rst
          return $ tyTm :<| rstTm

instance Quotable VRecord where
  type QuoteRes VRecord = Record

  quote lvl r = RecordSeq <$> mapM (quote lvl) r

instance Quotable VConstraint where
  type QuoteRes VConstraint = Constraint

  quote lvl = \case
    VTmEq s lhs rhs ty -> do
      lhsTm <- quote (shift s lvl) lhs
      rhsTm <- quote (shift s lvl) rhs
      tyTm <- quote (shift s lvl) ty
      return $ TmEq s lhsTm rhsTm tyTm

instance Quotable VProblem where
  type QuoteRes VProblem = Problem
  quote lvl = mapM (quote lvl)

quoteSpine :: LevelId -> Term -> Spine -> EvalResult Term
quoteSpine lvl stuck = \case
  SNil -> return stuck
  SApp s p -> do
    sTm <- quoteSpine lvl stuck s
    pTm <- quote lvl p
    return $ App sTm pTm
  SFirst s -> First <$> quoteSpine lvl stuck s
  SRest s -> Rest <$> quoteSpine lvl stuck s
  SJ fam p s -> do
    famTm <- quote (incrLvl $ incrLvl lvl) fam
    pTm <- quote lvl p
    sTm <- quoteSpine lvl stuck s
    return $ J famTm pTm sTm

instance Quotable Value where
  type QuoteRes Value = Term
  quote lvl = \case
    Neutral stuck spine -> do
      h <- case stuck of
        NVar l -> return $ Var $ toIndex lvl l
        NSplicing l -> return $ Splicing $ MVar (toIndex lvl l)
      quoteSpine lvl h spine
    VPi dom codHOAS -> do
      domTm <- quote lvl dom
      codTm <- quote lvl codHOAS
      return $ Pi domTm codTm
    VLam bodyHOAS -> Lam Nothing <$> quote lvl bodyHOAS
    VRecord tele -> Record <$> quote lvl tele
    VList list -> List <$> quote lvl list
    VId ty lhs rhs -> do
      tyTm <- quote lvl ty
      lhsTm <- quote lvl lhs
      rhsTm <- quote lvl rhs
      return $ Id tyTm lhsTm rhsTm
    VRefl -> return Refl
    VType l -> return $ Type l

-- Meta Language Readback
instance Quotable MetaHOAS where
  type QuoteRes MetaHOAS = MetaTerm
  quote lvl hoas = do
    val <- evalHOAS hoas liftMetaEnv
    quote (incrLvl lvl) val

quoteMSpine :: LevelId -> MetaTerm -> MetaSpine -> EvalResult MetaTerm
quoteMSpine lvl stuck = \case
  MSNil -> return stuck
  MSApp s p -> do
    sTm <- quoteMSpine lvl stuck s
    pTm <- quote lvl p
    return $ MApp sTm pTm
  MSForce s -> MForce <$> quoteMSpine lvl stuck s
  MSBind prev bHOAS tyHOAS -> do
    prevTm <- quoteMSpine lvl stuck prev
    bTm <- quote lvl bHOAS
    bindTyTm <- quote lvl tyHOAS
    return $ MLetIn prevTm bTm bindTyTm
  _ -> throwError $ Anyhow "unimplement"

instance Quotable MetaValue where
  type QuoteRes MetaValue = MetaTerm
  quote lvl = \case
    MNeutral h s -> quoteMSpine lvl (MVar $ toIndex lvl h) s
    MVPi dom e codHOAS -> do
      domTm <- quote lvl dom
      codTm <- quote lvl codHOAS
      return $ MPi domTm e codTm
    MVLam bodyHOAS -> MLam Nothing <$> quote lvl bodyHOAS
    MVF vty -> MF <$> quote lvl vty
    MVReturn v -> MReturn <$> quote lvl v
    MVTrigger e -> return $ MTrigger e
    MVCType l -> return $ MCType l
    MVU e cty -> MU e <$> quote lvl cty
    MVThunk c -> MThunk <$> quote lvl c
    MVVType l -> return $ MVType l
    _ -> throwError $ Anyhow "unimplement"
