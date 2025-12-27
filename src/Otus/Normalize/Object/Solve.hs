module Otus.Normalize.Object.Solve (
  solveProblem,
  isSolved,
  solveMeta,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Lazy (StateT (runStateT), gets, lift, modify)
import Data.Maybe

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

type SolveEnv = Seq (Maybe ObjValue)

buildSolveEnv :: Int -> SolveEnv
buildSolveEnv s = cycleTaking s [Nothing]

findMeta :: LevelId -> SolveMonad (Maybe ObjValue)
findMeta lvl =
  gets (@? lvl) >>= \case
    Just res -> return res
    Nothing -> throwError $ ObjUnknownMeta lvl

isSolved :: LevelId -> SolveMonad Bool
isSolved lvl = isJust <$> findMeta lvl

solveMeta :: LevelId -> ObjValue -> SolveMonad ()
solveMeta lvl val = modify $ update lvl (Just val)

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

type SolveMonad = StateT SolveEnv (ObjEvalResultT SolveResult)

runSolveMonad :: SolveMonad a -> SolveEnv -> ObjEvalResult (SolveResult (a, SolveEnv))
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
  res <- runSolveMonad (solveConstraintSet constrSet) (buildSolveEnv domainSize)
  case res of
    Conflict -> return Nothing
    Consistant _ _constrSet' -> undefined

solveConstraintSet :: ConstraintSeq -> SolveMonad ConstraintSeq
solveConstraintSet = seqMAppendM solveConstraint

solveConstraint :: VConstraint -> SolveMonad ConstraintSeq
solveConstraint (VTmEq ctxSize lhs rhs) = solveTmEqConstraint ctxSize lhs rhs

solveTmEqConstraint :: Int -> ObjValue -> ObjValue -> SolveMonad ConstraintSeq
solveTmEqConstraint ctxSize lhs rhs = do
  lhs' <- force lhs
  rhs' <- force rhs
  case (lhs', rhs') of
    (OVPi lDom lCls, OVPi rDom rCls) -> do
      domEq <- solveTmEqConstraint ctxSize lDom rDom
      bind <- freshBind
      lCod <- doEvalClosure bind lCls
      rCod <- doEvalClosure bind rCls
      codEq <- solveTmEqConstraint (ctxSize + 1) lCod rCod
      return $ domEq >< codEq
    (OVType, OVType) -> return empty
    (OVNeutral (ONRigid lh ls), OVNeutral (ONRigid rh rs)) ->
      if lh == rh then
        unifySpine ctxSize ls rs
      else
        return $ singleton $ VTmEq ctxSize lhs rhs
    (OVLam lCls, _) -> do
      bind <- freshBind
      lBody <- doEvalClosure bind lCls
      rBody <- doEvalApp rhs' bind
      solveTmEqConstraint (ctxSize + 1) lBody rBody
    (_, OVLam rCls) -> do
      bind <- freshBind
      lBody <- doEvalApp lhs' bind
      rBody <- doEvalClosure bind rCls
      solveTmEqConstraint (ctxSize + 1) lBody rBody
    _ -> throwError $ UnsolvableTmEq ctxSize lhs rhs
  where
    freshBind :: SolveMonad ObjValue
    freshBind = (vVar . LevelId) . shift ctxSize <$> metaSize

force :: ObjValue -> SolveMonad ObjValue
force = \case
  OVNeutral neu -> case neu of
    ONFlex lvl spine ->
      findMeta lvl >>= \case
        Just val -> liftEvalResult $ evalApp' val spine
        _ -> returnNeutral neu
    _ -> returnNeutral neu
  val -> return val

unifySpine :: Int -> ObjValueSeq -> ObjValueSeq -> SolveMonad ConstraintSeq
unifySpine ctxSize ls rs = case (ls, rs) of
  (Empty, Empty) -> return empty
  (lhs :<| ls', rhs :<| rs') -> do
    c <- solveTmEqConstraint ctxSize lhs rhs
    cs <- unifySpine ctxSize ls' rs'
    return $ c >< cs
  _ -> conflict
