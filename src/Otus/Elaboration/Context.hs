module Otus.Elaboration.Context (
  Context (..),
  pushTy,
  contains,
  findVTy,
  asEnv,
) where

import Data.Map

import Otus.Ast
import Otus.Common
import Otus.Normalize

data Context = Context
  { ctxSegs :: Seq (Value, Stage),
    idMap :: Map String LevelId
  }
  deriving (Eq, Show)

pushTy :: String -> Value -> Stage -> Context -> Maybe (LevelId, Context)
pushTy strId vTy stage (Context s idM) =
  if member strId idM then
    Nothing
  else
    let lvl = LevelId $ length s
    in return
         ( lvl,
           Context
             { ctxSegs = s |> (vTy, stage),
               idMap = insert strId lvl idM
             }
         )

contains :: String -> Context -> Bool
contains strId (Context _ idM) = member strId idM

findVTy :: String -> Context -> Maybe (IndexId, Value, Stage)
findVTy strId (Context s idM) = do
  lvl <- idM !? strId
  (val, stage) <- s @? lvl
  return (toIndex s lvl, val, stage)

asEnv :: Context -> Environment
asEnv (Context s _) = mapWithIndex (\idx _ -> vVar $ LevelId idx) s
