module Otus.Normalize.Solve (
  Signature (..),
  solveSignature,
) where

import Control.Monad (mapAndUnzipM, when)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Value

solveConstraint :: VConstraint -> SolveMonad ([VConstraint], Bool)
solveConstraint = undefined

solveConstraints :: [VConstraint] -> SolveMonad ([VConstraint], Bool)
solveConstraints constrs = do
  (simplified, solve) <- mapAndUnzipM solveConstraint constrs
  if or solve then do
    (res, _) <- solveConstraints $ concat simplified
    return (res, True)
  else
    return (concat simplified, False)

solveMetaDef :: LevelId -> VMetaDefinition -> SolveMonad (VMetaDefinition, Bool)
solveMetaDef lvl = \case
  VUnsolved -> return (VUnsolved, False)
  VSolved val constrs
    | null constrs -> return (VSolved val constrs, False)
    | otherwise -> do
        (simplified, solve) <- solveConstraints constrs
        when (null simplified) $ doAssignMeta lvl val
        return (VSolved val simplified, solve)

solveSignature :: LevelId -> VSignature -> SolveMonad VSignature
solveSignature (LevelId lvl) (VSig defs) = do
  (simplified, solve) <- mapAndUnzipM (\(idx, def) -> solveMetaDef (LevelId $ lvl + idx) def) $ enumurate defs
  if or solve then do
    solveSignature (LevelId lvl) (VSig simplified)
  else
    return $ VSig simplified
