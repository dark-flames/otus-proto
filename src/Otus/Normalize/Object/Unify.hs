module Otus.Normalize.Object.Unify (
  solveProblem,
  isSolved,
  solveMeta,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Lazy (StateT (runStateT), gets, lift, modify)

import qualified Data.IntMap as IM

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Env
import Otus.Normalize.Object.Error
import Otus.Normalize.Object.Eval
import Otus.Normalize.Object.Value

-- Constraint Solve

data WFProblem = WFProblem
  { pCtx :: Int,
    wfProblem :: VProblem
  }
  deriving (Eq, Show)

doFindMeta :: MetaId -> SolveMonad MetaEntry
doFindMeta m =
  gets (@? unMeta m) >>= \case
    Just res -> return res
    Nothing -> throwError $ ObjUnknownMeta m

isSolved :: MetaId -> SolveMonad Bool
isSolved m =
  doFindMeta m
    >>= \case
      Solved _ -> return True
      _ -> return False

doSolveMeta :: MetaId -> ObjValue -> SolveMonad ()
doSolveMeta m val = modify $ solveMeta m val

metaSize :: SolveMonad Int
metaSize = gets size

newtype MetaSubst = MSubst (Seq (Maybe ObjValue))
  deriving (Eq, Show)

-- Control
data SolveResult res
  = Consistant Bool res
  | Conflict
  deriving (Eq, Show)

instance Functor SolveResult where
  fmap f = \case
    Conflict -> Conflict
    Consistant m r -> Consistant m $ f r

instance Applicative SolveResult where
  pure = Consistant False

  Conflict <*> _ = Conflict
  _ <*> Conflict = Conflict
  (Consistant mf f) <*> (Consistant ma a) = Consistant (mf || ma) $ f a

instance Monad SolveResult where
  Conflict >>= _ = Conflict
  Consistant ma r >>= f = case f r of
    Conflict -> Conflict
    Consistant mb r' -> Consistant (ma || mb) r'

type SolveMonad = StateT MetaContext (ObjEvalResultT SolveResult)

runSolveMonad :: SolveMonad a -> MetaContext -> ObjEvalResult (SolveResult (a, MetaContext))
runSolveMonad m env = case runResultT (runStateT m env) of
  Conflict -> return Conflict
  Consistant modified (Success r) -> return $ Consistant modified r
  Consistant _ (Failure e) -> throwError e

liftEvalResult :: ObjEvalResult a -> SolveMonad a
liftEvalResult eval = lift $ ResultT (pure eval)

doEvalApp :: ObjValue -> ObjValue -> SolveMonad ObjValue
doEvalApp fn arg = liftEvalResult $ evalApp fn arg

doEvalClosure :: ObjValue -> ObjClosure -> SolveMonad ObjValue
doEvalClosure arg cls = liftEvalResult $ evalClosure arg cls

conflict :: SolveMonad a
conflict = lift $ ResultT Conflict

type ConstraintSeq = Seq VConstraint

solveProblem :: WFProblem -> ObjEvalResult (Maybe (WFProblem, VRecord))
solveProblem (WFProblem domainSize (VProb constrSet)) = do
  res <- runSolveMonad (solveConstraintSet constrSet) (buildMetaCtx domainSize)
  case res of
    Conflict -> return Nothing
    Consistant _ _constrSet' -> undefined

solveConstraintSet :: ConstraintSeq -> SolveMonad ConstraintSeq
solveConstraintSet = seqMAppendM solveConstraint

solveConstraint :: VConstraint -> SolveMonad ConstraintSeq
solveConstraint (VTmEq ctxSize lhs rhs) = unifyTm ctxSize lhs rhs

unifyTm :: Int -> ObjValue -> ObjValue -> SolveMonad ConstraintSeq
unifyTm ctxSize lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (OVPi lDom lCls, OVPi rDom rCls) -> do
      domEq <- unifyTm ctxSize lDom rDom
      bind <- freshBind
      lCod <- doEvalClosure bind lCls
      rCod <- doEvalClosure bind rCls
      codEq <- unifyTm (ctxSize + 1) lCod rCod
      return $ domEq >< codEq
    (OVType, OVType) -> return empty
    (OVNeutral (ONRigid lh ls), OVNeutral (ONRigid rh rs)) ->
      if lh == rh then
        unifySpine ctxSize ls rs
      else
        conflict
    (OVNeutral (ONFlex _ _), OVNeutral (ONFlex _ _)) -> keepConstraint
    (OVLam lCls, _) -> do
      bind <- freshBind
      lBody <- doEvalClosure bind lCls
      rBody <- doEvalApp rhs' bind
      unifyTm (ctxSize + 1) lBody rBody
    (_, OVLam rCls) -> do
      bind <- freshBind
      lBody <- doEvalApp lhs' bind
      rBody <- doEvalClosure bind rCls
      unifyTm (ctxSize + 1) lBody rBody
    (OVNeutral (ONFlex lh ls), _) -> solve ctxSize lh ls rhs'
    (_, OVNeutral (ONFlex rh rs)) -> solve ctxSize rh rs lhs'
    _ -> throwError $ UnsolvableTmEq ctxSize lhs rhs
  where
    freshBind :: SolveMonad ObjValue
    freshBind = (vVar . LevelId) . shift ctxSize <$> metaSize

    keepConstraint :: SolveMonad ConstraintSeq
    keepConstraint = return $ singleton $ VTmEq ctxSize lhs rhs

force :: ObjValue -> SolveMonad ObjValue
force = \case
  OVNeutral neu -> case neu of
    ONFlex lvl spine ->
      doFindMeta lvl >>= \case
        Solved val -> liftEvalResult $ evalApp' val spine
        UnSolved -> returnNeutral neu
    _ -> returnNeutral neu
  val -> return val

unifySpine :: Int -> ObjValueSeq -> ObjValueSeq -> SolveMonad ConstraintSeq
unifySpine ctxSize ls rs = case (ls, rs) of
  (Empty, Empty) -> return empty
  (lhs :<| ls', rhs :<| rs') -> do
    c <- unifyTm ctxSize lhs rhs
    cs <- unifySpine ctxSize ls' rs'
    return $ c >< cs
  _ -> conflict

data PartialRenaming = PRen
  { prDom :: Int,
    prCod :: Int,
    prRen :: IM.IntMap LevelId
  }
  deriving (Eq, Show)

liftPRenaming :: PartialRenaming -> PartialRenaming
liftPRenaming (PRen dom cod ren) =
  PRen (dom + 1) (cod + 1) (IM.insert cod (LevelId dom) ren)

invert :: Int -> ObjValueSeq -> SolveMonad (Maybe PartialRenaming)
invert cod sp =
  go sp >>= \case
    Just (dom, ren) -> return $ Just $ PRen dom cod ren
    _ -> return Nothing
  where
    go :: ObjValueSeq -> SolveMonad (Maybe (Int, IM.IntMap LevelId))
    go Empty = return $ Just (0, mempty)
    go (sp' :|> param) =
      go sp' >>= \case
        Just (dom, ren) -> do
          param' <- force param
          case param' of
            OVNeutral (ONRigid (LevelId x) Empty) ->
              if IM.notMember x ren then
                return $ Just (dom + 1, IM.insert x (LevelId dom) ren)
              else
                conflict
            _ -> return Nothing
        _ -> return Nothing

rename :: Int -> MetaId -> PartialRenaming -> ObjValue -> SolveMonad ObjTerm
rename ctxSize m = go
  where
    goSpine :: PartialRenaming -> ObjTerm -> ObjValueSeq -> SolveMonad ObjTerm
    goSpine pren h = \case
      Empty -> return h
      a :<| sp -> do
        a' <- go pren a
        goSpine pren (OApp h a') sp
    go :: PartialRenaming -> ObjValue -> SolveMonad ObjTerm
    go pren t =
      force t
        >>= \case
          OVNeutral neu -> case neu of
            ONFlex m' sp ->
              if m == m' then
                conflict -- todo: keep?
              else
                goSpine pren (OMeta m) sp
            ONRigid (LevelId x) sp -> case IM.lookup x (prRen pren) of
              Nothing -> conflict -- todo: keep?
              Just lvl -> goSpine pren (OVar $ toIndex ctxSize lvl) sp
          OVLam cls -> do
            body <- doEvalClosure (vVar $ LevelId $ prCod pren) cls
            bodyTm <- go (liftPRenaming pren) body
            return $ OLam bodyTm
          OVPi a cls -> do
            aTm <- go pren a
            b <- doEvalClosure (vVar $ LevelId $ prCod pren) cls
            bTm <- go (liftPRenaming pren) b
            return $ OPi aTm bTm
          OVType -> return OType

lams :: Int -> ObjTerm -> ObjTerm
lams = \case
  x | x > 0 -> OLam . lams (x - 1)
  _ -> id

solve :: Int -> MetaId -> ObjValueSeq -> ObjValue -> SolveMonad ConstraintSeq
solve ctxSize m sp rhs =
  invert ctxSize sp
    >>= \case
      Just pren -> do
        rhsTm <- rename ctxSize m pren rhs
        solution <- liftEvalResult $ evaluateObj (lams (size sp) rhsTm) eempty
        doSolveMeta m solution
        return mempty
      _ -> return $ singleton $ VTmEq ctxSize (OVNeutral $ ONFlex m sp) rhs
