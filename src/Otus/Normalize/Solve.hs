module Otus.Normalize.Solve (
  solveSignature,
) where

import Control.Monad (mapAndUnzipM)
import Control.Monad.State.Strict (lift)

import Otus.Ast
import Otus.Normalize.Control
import {-# SOURCE #-} Otus.Normalize.Eval
import Otus.Normalize.Value

solveConstraint :: VConstraint -> EvalMonad ([VConstraint], Bool)
solveConstraint = undefined

solveConstraints :: [VConstraint] -> EvalMonad ([VConstraint], Bool)
solveConstraints constrs = do
  (simplified, solve) <- mapAndUnzipM solveConstraint constrs
  if or solve then do
    (res, _) <- solveConstraints $ concat simplified
    return (res, True)
  else
    return (concat simplified, False)

solveMetaDef :: LevelId -> VMetaDefinition -> EvalMonad (VMetaDefinition, Bool)
solveMetaDef bound = \case
  VMUnsolved -> return (VMUnsolved, False)
  VMGuarded cls constrs -> do
    (simplified, solve) <- solveConstraints constrs
    if null simplified then do
      args <- doCollectArgs bound
      val <- lift $ evalClosure' args cls
      doPushMetaView $ SolvedMeta val
      return (VMSolved cls, True)
    else
      return (VMGuarded cls simplified, solve)
  VMSolved cls -> do
    args <- doCollectArgs bound
    val <- lift $ evalClosure' args cls
    doPushMetaView $ SolvedMeta val
    return (VMSolved cls, False)

solveSignature :: VSignature -> EvalMonad VSignature
solveSignature (VSig defs) = do
  bound <- doGetEnvLevel
  (simplified, solve) <- mapAndUnzipM (solveMetaDef bound) defs
  -- run until nothing solved : to be optimized
  if or solve then do
    solveSignature (VSig simplified)
  else
    return $ VSig simplified
