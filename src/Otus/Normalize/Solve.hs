module Otus.Normalize.Solve (
  Signature (..),
  solveGuardedSubst,
) where

import Control.Monad (mapAndUnzipM, when)
import Control.Monad.State (StateT)

import Otus.Ast
import Otus.Normalize.Control
import Otus.Normalize.Value

newtype Signature = Signature
  { envLvl :: LevelId
  }

type SolveResult = StateT Signature EvalResult

doAssignMeta :: LevelId -> Value -> SolveResult ()
doAssignMeta = undefined

solveConstraint :: VConstraint -> SolveResult ([VConstraint], Bool)
solveConstraint = undefined

solveConstraints :: [VConstraint] -> SolveResult ([VConstraint], Bool)
solveConstraints constrs = do
  (simplified, solve) <- mapAndUnzipM solveConstraint constrs
  if or solve then do
    (res, _) <- solveConstraints $ concat simplified
    return (res, True)
  else
    return (concat simplified, False)

solveGuardedSubstSeg :: LevelId -> VGuardedSubstSeg -> SolveResult (VGuardedSubstSeg, Bool)
solveGuardedSubstSeg lvl = \case
  VUnsolved -> return (VUnsolved, False)
  VSolved val constrs
    | null constrs -> return (VSolved val constrs, False)
    | otherwise -> do
        (simplified, solve) <- solveConstraints constrs
        when (null simplified) $ doAssignMeta lvl val
        return (VSolved val simplified, solve)

solveGuardedSubst :: LevelId -> VGuardedSubstitution -> SolveResult VGuardedSubstitution
solveGuardedSubst (LevelId lvl) (VGSubst segs) = do
  let
    indexed = zipWith (\idx seg -> (LevelId $ lvl + idx, seg)) [0 ..] segs
  (simplified, solve) <- mapAndUnzipM (uncurry solveGuardedSubstSeg) indexed
  if or solve then do
    solveGuardedSubst (LevelId lvl) (VGSubst simplified)
  else
    return $ VGSubst simplified
