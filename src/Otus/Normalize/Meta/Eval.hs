module Otus.Normalize.Meta.Eval (
  evaluateMeta,
  evalMetaType,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Meta.Error
import Otus.Normalize.Meta.Value

evaluateMeta :: MetaTerm -> MetaEnv -> MetaEvalResult MetaValue
evaluateMeta tm env = case tm of
  MVar idx -> case env @? idx of
    Just val -> return val
    Nothing -> throwError $ MetaUnboundIndex idx
  MLam body -> return $ MVLam $ MetaClosure env body
  MApp fn arg -> do
    vFn <- go fn
    vArg <- go arg
    evalMetaApp vFn vArg
  MOk inner -> MVOk <$> go inner
  MErr -> return MVErr
  MBind prev handler -> do
    vPrev <- go prev
    vHandler <- go handler
    evalMetaBind vPrev vHandler
  _ -> undefined
  where
    go tm' = evaluateMeta tm' env

evalMetaType :: MetaType -> MetaEvalResult MetaVType
evalMetaType = \case
  MFn dom cod -> do
    vDom <- evalMetaType dom
    vCod <- evalMetaType cod
    return $ MVFn vDom vCod
  MDyn inner -> MVDyn <$> evalMetaType inner
  _ -> undefined

evalMetaClosure :: MetaClosure -> MetaValue -> MetaEvalResult MetaValue
evalMetaClosure (MetaClosure env tm) arg = evaluateMeta tm (env |> arg)

evalMetaApp :: MetaValue -> MetaValue -> MetaEvalResult MetaValue
evalMetaApp vFn vArg = case vFn of
  MVLam cls -> evalMetaClosure cls vArg
  MVNeutral neu -> returnNeutral $ metaNeutralApp neu vArg
  _ -> throwError MetaAppOnNonFn

evalMetaBind :: MetaValue -> MetaValue -> MetaEvalResult MetaValue
evalMetaBind prev handler = case prev of
  MVOk vRes -> evalMetaApp handler vRes
  MVErr -> return MVErr
  MVNeutral neu -> returnNeutral $ MNBind neu handler
  _ -> throwError MetaBindOnNonDyn
