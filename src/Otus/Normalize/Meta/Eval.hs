module Otus.Normalize.Meta.Eval (
  evaluateMeta,
  evalMetaType,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State.Lazy (get, lift)

import Otus.Ast
import Otus.Common
import Otus.Normalize.Control
import Otus.Normalize.Meta.Error
import Otus.Normalize.Meta.Value
import Otus.Normalize.Object.Eval
import Otus.Normalize.Object.Value

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
  MGuarded tele sig subst objTerm -> do
    (vTele, vSig, vSubst) <- evalObjEvalMonad $ do
      vTele <- evalTelescope tele
      vSig <- evalSignature sig
      env' <- get
      vSubst <- lift $ evalSubstitution subst env'
      return (vTele, vSig, vSubst)
    return $ MVConsistent vTele vSig vSubst (ObjClosure empty objTerm)
  MErr -> return MVErr
  where
    go tm' = evaluateMeta tm' env

evalMetaType :: MetaType -> MetaEvalResult MetaVType
evalMetaType = \case
  MFn dom cod -> do
    vDom <- evalMetaType dom
    vCod <- evalMetaType cod
    return $ MVFn vDom vCod
  MInner tele objTy ->
    uncurry MVInner
      <$> evalObjEvalMonad
        ( do
            vTele <- evalTelescope tele
            vTy <- doEvaluate objTy
            return (vTele, vTy)
        )

evalMetaClosure :: MetaClosure -> MetaValue -> MetaEvalResult MetaValue
evalMetaClosure (MetaClosure env tm) arg = evaluateMeta tm (env |> arg)

evalMetaApp :: MetaValue -> MetaValue -> MetaEvalResult MetaValue
evalMetaApp vFn vArg = case vFn of
  MVLam cls -> evalMetaClosure cls vArg
  MVNeutral neu -> returnNeutral $ metaNeutralApp neu vArg
  _ -> throwError MetaAppOnNonFn
