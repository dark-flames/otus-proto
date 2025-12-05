module Otus.Normalize.Object.Solve (
  solveSignature,
) where

import Control.Monad (when)
import Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), gets, modify)
import Data.Maybe (fromJust)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Object.Eval
import Otus.Normalize.Object.Value

data CachedClosure
  = Unevaluated Closure
  | Evaluated Closure ObjValue
  deriving (Eq, Show)

asClosure :: CachedClosure -> Closure
asClosure = \case
  Unevaluated cls -> cls
  Evaluated cls _ -> cls

data MetaState
  = MSUnknown LevelId
  | MSGuarded LevelId CachedClosure (Seq VConstraint)
  | MSSolved LevelId CachedClosure
  deriving (Eq, Show)

type MetaStateSeq = Seq MetaState

data Problem = Problem LevelId MetaStateSeq
  deriving (Eq, Show)

adjustState :: LevelId -> (MetaState -> MetaState) -> Problem -> Problem
adjustState idx h (Problem base s) = Problem base $ adjust h (sub idx base) s

type SolveMonad = StateT Problem EvalResult

data SolveResult
  = NoModification
  | Modified LevelId
  | Conflict

instance Semigroup SolveResult where
  Conflict <> _ = Conflict
  _ <> Conflict = Conflict
  NoModification <> r = r
  l <> NoModification = l
  Modified lvlL <> Modified lvlR = Modified $ min lvlL lvlR

andThen :: SolveResult -> SolveMonad SolveResult -> SolveMonad SolveResult
andThen = \case
  Conflict -> const $ return Conflict
  NoModification -> id
  curRes -> fmap (curRes <>)

notConflict :: SolveResult -> Bool
notConflict = \case
  Conflict -> False
  _ -> True

fromVSig :: LevelId -> VSignature -> Problem
fromVSig lvl (VSig defs) = Problem lvl $ go lvl defs
  where
    go _ Empty = empty
    go l (def :<| rest) = case def of
      VMUnsolved -> MSUnknown l <| go (lvl + 1) rest
      VMGuarded cls constrs -> MSGuarded l (Unevaluated cls) constrs <| go (lvl + 1) rest
      VMSolved cls -> MSSolved l (Unevaluated cls) <| go (lvl + 1) rest

toVSig :: Problem -> VSignature
toVSig (Problem _ states) = VSig $ f <$> states
  where
    f = \case
      MSUnknown _ -> VMUnsolved
      MSGuarded _ cached constrs -> VMGuarded (asClosure cached) constrs
      MSSolved _ cached -> VMSolved (asClosure cached)

-- Control
getMetaState :: LevelId -> SolveMonad MetaState
getMetaState lvl = gets (\(Problem base s) -> fromJust $ s @? sub lvl base)

getBaseLevel :: SolveMonad LevelId
getBaseLevel = gets (\(Problem base _) -> base)

getSize :: SolveMonad Int
getSize = gets (\(Problem _ s) -> size s)

assignSolvedMeta :: LevelId -> CachedClosure -> SolveMonad ()
assignSolvedMeta lvl cls =
  modify (adjustState lvl (const $ MSSolved lvl cls))

-- Solve
solveSignature :: LevelId -> VSignature -> EvalResult (Maybe VSignature)
solveSignature lvl vSig = do
  let problem = fromVSig lvl vSig
  (noConflict, problem') <- runStateT doSolve problem
  if noConflict then
    return $ Just $ toVSig problem'
  else
    return Nothing

doSolve :: SolveMonad Bool
doSolve = notConflict <$> (getBaseLevel >>= go)
  where
    -- run solve step from the boundary until no modification
    go lvl = do
      r <- solveOnceFrom lvl
      case r of
        Modified bound -> go bound
        _ -> return r

--- solve defs from the given level once time
solveOnceFrom :: LevelId -> SolveMonad SolveResult
solveOnceFrom lvl = getSize >>= go lvl
  where
    go _ 0 = return NoModification
    go l fuel = solveSingle l >>= (`andThen` go (incrLvl l) (fuel - 1))

solveSingle :: LevelId -> SolveMonad SolveResult
solveSingle lvl =
  getMetaState lvl >>= \case
    MSGuarded _ cls constrs -> do
      (simplified, res) <- solveConstraints constrs
      when (null simplified) $ assignSolvedMeta lvl cls
      return $ res <> Modified lvl
    _ -> return NoModification

solveConstraints :: Seq VConstraint -> SolveMonad (Seq VConstraint, SolveResult)
solveConstraints constrs = do
  (simplified, res) <- solveConstraintsOnce constrs
  case res of
    Modified _ -> do
      (simplified', res') <- solveConstraints simplified
      return (simplified', res <> res')
    _ -> return (simplified, res)

--- solve given constraints
--- effect: update previous meta state if any meta was solved
solveConstraintsOnce :: Seq VConstraint -> SolveMonad (Seq VConstraint, SolveResult)
solveConstraintsOnce = seqFoldlM go (empty, NoModification)
  where
    go (simplified, res) constr =
      if notConflict res then do
        (constrs, res') <- solveConstraint constr
        return (simplified >< constrs, res <> res')
      else
        return (simplified |> constr, res)

solveConstraint :: VConstraint -> SolveMonad (Seq VConstraint, SolveResult)
solveConstraint (VTmEq vTele lhs rhs vTy) = solveTmEq vTele (lhs, rhs) vTy
solveConstraint (VTyEq vTele lhs rhs) = solveTyEq vTele (lhs, rhs)

solveTyEq :: VTelescope -> (ObjValue, ObjValue) -> SolveMonad (Seq VConstraint, SolveResult)
solveTyEq _ = undefined

solveTmEq :: VTelescope -> (ObjValue, ObjValue) -> ObjValue -> SolveMonad (Seq VConstraint, SolveResult)
solveTmEq vTele (lhs, rhs) vty = case vty of
  OVPi vDom codCls -> do
    (vCod, arg) <- doEvalClsFresh codCls
    vLhs <- doEvalApp lhs arg
    vRhs <- doEvalApp rhs arg
    let vTele' = vTele |> vDom
    solveTmEq vTele' (vLhs, vRhs) vCod
  _ -> returnConflict
  where
    returnConflict = return (singleton $ VTmEq vTele lhs rhs vty, Conflict)

-- evaluate
doEvalClsFresh :: Closure -> SolveMonad (ObjValue, ObjValue)
doEvalClsFresh cls = lift $ evalClosureFresh cls

doEvalApp :: ObjValue -> ObjValue -> SolveMonad ObjValue
doEvalApp fun arg = lift $ evalApp fun arg
