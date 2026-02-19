```haskell
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
data SolveState = SolveState
  { metaCtx :: MetaContext,
    modified :: Bool
  }
  deriving (Eq, Show)

getMetaCtx :: SolveMonad MetaContext
getMetaCtx = gets metaCtx

modifyMetaCtx :: (MetaContext -> MetaContext) -> SolveMonad ()
modifyMetaCtx f = modify (\(SolveState ctx m) -> SolveState (f ctx) m)

doFindMeta :: MetaId -> SolveMonad MetaEntry
doFindMeta m =
  getMetaCtx
    >>= ( \case
            Just res -> return res
            Nothing -> throwError $ ObjUnknownMeta m
        )
      . (@? unMeta m)

isSolved :: MetaId -> SolveMonad Bool
isSolved m =
  doFindMeta m
    >>= \case
      Solved _ -> return True
      _ -> return False

doSolveMeta :: MetaId -> Value -> SolveMonad ()
doSolveMeta m val = do
  modifyMetaCtx $ solveMeta m val
  setModified
  lift $ ResultT $ Consistent (Success ())

metaSize :: SolveMonad Int
metaSize = size <$> getMetaCtx

isModified :: SolveMonad Bool
isModified = gets modified

resetModified :: SolveMonad ()
resetModified = modify (\(SolveState ctx _) -> SolveState ctx False)

setModified :: SolveMonad ()
setModified = modify (\(SolveState ctx _) -> SolveState ctx True)

newtype MetaSubst = MSubst (Seq (Maybe Value))
  deriving (Eq, Show)

-- Control
data SolveResult res
  = Consistent res
  | Conflict
  deriving (Eq, Show)

instance Functor SolveResult where
  fmap f = \case
    Conflict -> Conflict
    Consistent r -> Consistent $ f r

instance Applicative SolveResult where
  pure = Consistent

  Conflict <*> _ = Conflict
  _ <*> Conflict = Conflict
  (Consistent f) <*> (Consistent a) = Consistent $ f a

instance Monad SolveResult where
  Conflict >>= _ = Conflict
  Consistent r >>= f = case f r of
    Conflict -> Conflict
    Consistent r' -> Consistent r'

type SolveMonad = StateT SolveState (ObjEvalResultT SolveResult)

runSolveMonad :: SolveMonad a -> MetaContext -> ObjEvalResult (SolveResult (a, SolveState))
runSolveMonad m env = case runResultT (runStateT m (SolveState env False)) of
  Conflict -> return Conflict
  Consistent (Success r) -> return $ Consistent r
  Consistent (Failure e) -> throwError e

liftEvalResult :: ObjEvalResult a -> SolveMonad a
liftEvalResult eval = lift $ ResultT (pure eval)

doEvalApp :: Value -> Value -> SolveMonad Value
doEvalApp fn arg = liftEvalResult $ evalApp fn arg

doEvalMetaClosure :: Value -> ObjClosure -> SolveMonad Value
doEvalMetaClosure arg cls = liftEvalResult $ evalClosure arg cls

conflict :: SolveMonad a
conflict = lift $ ResultT Conflict

type ConstraintSeq = Seq VConstraint

solveProblem :: MetaContext -> VProblem -> ObjEvalResult (Maybe (VProblem, MetaContext))
solveProblem mctx (VProb s) = do
  res <- runSolveMonad (solveConstraintSet s) mctx
  case res of
    Conflict -> return Nothing
    Consistent (s', SolveState mctx' _) -> return $ Just (VProb s', mctx')

solveConstraintSet :: ConstraintSeq -> SolveMonad ConstraintSeq
solveConstraintSet s = do
  res <- seqMAppendM solveConstraint s
  isModified
    >>= \case
      True -> do
        resetModified
        solveConstraintSet res
      False -> return res

solveConstraint :: VConstraint -> SolveMonad ConstraintSeq
solveConstraint (VTmEq ctxSize lhs rhs) = unifyTm ctxSize lhs rhs

unifyTm :: Int -> Value -> Value -> SolveMonad ConstraintSeq
unifyTm ctxSize lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (OVPi lDom lCls, OVPi rDom rCls) -> do
      domEq <- unifyTm ctxSize lDom rDom
      bind <- freshBind
      lCod <- doEvalMetaClosure bind lCls
      rCod <- doEvalMetaClosure bind rCls
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
      lBody <- doEvalMetaClosure bind lCls
      rBody <- doEvalApp rhs' bind
      unifyTm (ctxSize + 1) lBody rBody
    (_, OVLam rCls) -> do
      bind <- freshBind
      lBody <- doEvalApp lhs' bind
      rBody <- doEvalMetaClosure bind rCls
      unifyTm (ctxSize + 1) lBody rBody
    (OVNeutral (ONFlex lh ls), _) -> solve ctxSize lh ls rhs'
    (_, OVNeutral (ONFlex rh rs)) -> solve ctxSize rh rs lhs'
    _ -> throwError $ UnsolvableTmEq ctxSize lhs rhs
  where
    freshBind :: SolveMonad Value
    freshBind = (vVar . LevelId) . shift ctxSize <$> metaSize

    keepConstraint :: SolveMonad ConstraintSeq
    keepConstraint = return $ singleton $ VTmEq ctxSize lhs rhs

force :: Value -> SolveMonad Value
force = \case
  OVNeutral neu -> case neu of
    ONFlex lvl spine ->
      doFindMeta lvl >>= \case
        Solved val -> liftEvalResult $ evalApp' val spine
        UnSolved -> returnNeutral neu
    _ -> returnNeutral neu
  val -> return val

unifySpine :: Int -> ValueSeq -> ValueSeq -> SolveMonad ConstraintSeq
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

invert :: Int -> ValueSeq -> SolveMonad (Maybe PartialRenaming)
invert cod sp =
  go sp >>= \case
    Just (dom, ren) -> return $ Just $ PRen dom cod ren
    _ -> return Nothing
  where
    go :: ValueSeq -> SolveMonad (Maybe (Int, IM.IntMap LevelId))
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

rename :: Int -> MetaId -> PartialRenaming -> Value -> SolveMonad Term
rename ctxSize m = go
  where
    goSpine :: PartialRenaming -> Term -> ValueSeq -> SolveMonad Term
    goSpine pren h = \case
      Empty -> return h
      a :<| sp -> do
        a' <- go pren a
        goSpine pren (App h a') sp
    go :: PartialRenaming -> Value -> SolveMonad Term
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
              Just lvl -> goSpine pren (Var $ toIndex ctxSize lvl) sp
          OVLam cls -> do
            body <- doEvalMetaClosure (vVar $ LevelId $ prCod pren) cls
            bodyTm <- go (liftPRenaming pren) body
            return $ Lam bodyTm
          OVPi a cls -> do
            aTm <- go pren a
            b <- doEvalMetaClosure (vVar $ LevelId $ prCod pren) cls
            bTm <- go (liftPRenaming pren) b
            return $ Pi aTm bTm
          OVType -> return OType

lams :: Int -> Term -> Term
lams = \case
  x | x > 0 -> Lam . lams (x - 1)
  _ -> id

solve :: Int -> MetaId -> ValueSeq -> Value -> SolveMonad ConstraintSeq
solve ctxSize m sp rhs =
  invert ctxSize sp
    >>= \case
      Just pren -> do
        rhsTm <- rename ctxSize m pren rhs
        solution <- liftEvalResult $ evaluateObj (lams (size sp) rhsTm) eempty
        doSolveMeta m solution
        return mempty
      _ -> return $ singleton $ VTmEq ctxSize (OVNeutral $ ONFlex m sp) rhs