module Otus.TypeCheck.Tyck (
  inferValue,
  inferComputation,
  checkValue,
  checkComputation,
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Context
import Otus.TypeCheck.Conv
import Otus.TypeCheck.Error
import Otus.TypeCheck.Judgement

inferValue :: Context -> MetaTerm -> TypeCheckResult WfValue
inferValue ctx preTm = case preTm of
  MVar idx -> case ctx @? idx of
    Just (MetaTy mty) ->
      return $
        WfValue
          { vtm = preTm,
            vtyOf = mty
          }
    Just (ObjTy _) -> throwError CannotInferObjAsMeta
    _ -> throwError CannotInferIndex
  MU _ c -> do
    (cTy, l) <- inferComputationTy ctx c
    return $
      WfValue
        { vtm = ctm cTy,
          vtyOf = MVVType l
        }
  MVType l ->
    return $
      WfValue
        { vtm = preTm,
          vtyOf = MVVType (l + 1)
        }
  _ -> throwError $ CannotInferValue preTm

inferComputation :: Context -> MetaTerm -> TypeCheckResult WfComputation
inferComputation ctx preTm = case preTm of
  MPi dom eff cod -> do
    -- Γ |- Dom vtype @ domL
    (domVTy, domL) <- inferValueTy ctx dom
    vDomVTy <- mapEvalResult $ evaluateMetaValue (vtm domVTy) (ctxEnv ctx)
    -- Γ, Dom |- Cod ctype @ codL ! e
    (codCTy, codL) <- inferComputationTy (ctx |:> vDomVTy) cod

    -- Γ |- Pi(Dom, eff, Cod) ctype @ domL /\ codL ! e
    return $
      WfComputation
        { ctm = MPi (vtm domVTy) eff (ctm codCTy),
          effOf = effOf codCTy,
          ctyOf = MVCType (max domL codL)
        }
  MApp f p -> do
    -- Γ |- f :> e ! Pi A e' B
    fTm <- inferComputation ctx f
    let e = effOf fTm
    case ctyOf fTm of
      MVPi dom e' cls -> do
        -- Γ |- p<: A
        pTm <- checkValue ctx p dom
        vP <- mapEvalResult $ evaluateMetaValue (vtm pTm) (ctxEnv ctx)
        -- Γ |- B[id, p] ctype
        res <- mapEvalResult $ evaluateMetaClosure vP cls
        -- Γ |- f p :> e \/ e' ! B[id, p]
        return $
          WfComputation
            { ctm = MApp (ctm fTm) (vtm pTm),
              effOf = e \/ e',
              ctyOf = res
            }
      _ -> throwError $ ExpectedToBeMetaPi preTm
  MF a -> do
    -- Γ |- A vtype @ l
    (aTm, l) <- inferValueTy ctx a
    -- Γ |- F(A): empty ! CTy l
    return $
      WfComputation
        { ctm = vtm aTm,
          effOf = mempty,
          ctyOf = MVCType l
        }
  MTrigger e c -> do
    -- Γ |- C ctype
    (cTy, _) <- inferComputationTy ctx c
    vCTy <- mapEvalResult $ evaluateMetaComputation (ctm cTy) (ctxEnv ctx)

    -- Γ |- trigger(e, C) : {e} ! C
    return $
      WfComputation
        { ctm = MTrigger e (ctm cTy),
          effOf = singletonEff e,
          ctyOf = vCTy
        }
  MLetIn prev bind bindTy -> do
    -- Γ |- prev :> e ! F(A)
    prevTm <- inferComputation ctx prev
    let e = effOf prevTm
    case ctyOf prevTm of
      MVF vTy -> do
        let env = ctxEnv ctx
        -- Γ, U(e, F(A)) |- B ctype
        (kleisliExt, _) <- inferComputationTy (ctx |:> MVU e (MVF vTy)) bindTy
        -- Γ, A |- drop(1), thunk(return v0) => Γ, U(e, F(A))
        let thunked = MVThunk (MVReturn $ mvvar $ envLevel env)
        -- Γ, A |- B[drop(1), thunk(return v0)] ctype
        bindCTy <-
          mapEvalResult $
            evaluateMetaComputation
              (ctm kleisliExt)
              (env ||> thunked)
        -- Γ, A |- bind <: e' ! B[drop(1), thunk(return v0)]
        bindTm <- checkComputation (ctx |:> vTy) bind bindCTy
        let e' = effOf bindTm
        -- Γ |- id, thunk prev => Γ, U(e, F(A))
        vPrevTm <- mapEvalResult $ evaluateMetaComputation (ctm prevTm) env
        -- Γ |- B[id, thunk prev] ctype
        resultCTy <-
          mapEvalResult $
            evaluateMetaComputation
              (ctm kleisliExt)
              (env ||> MVThunk vPrevTm)
        -- Γ |- let prev in bind :> e \/ e' ! B[thunk prev]
        return $
          WfComputation
            { ctm = MLetIn (ctm prevTm) (ctm bindTm) (ctm kleisliExt),
              effOf = e \/ e',
              ctyOf = resultCTy
            }
      cTy -> throwError $ CannotBindOn prev cTy
  MForce v -> do
    -- Γ |- v :> U e C
    vTm <- inferValue ctx v
    case vtyOf vTm of
      -- Γ |- force(v) :> e ! C
      MVU eff cTy ->
        return $
          WfComputation
            { ctm = vtm vTm,
              effOf = eff,
              ctyOf = cTy
            }
      t -> throwError $ CannotCheckAsThunk v t
  -- Γ |- CTy(l) :> CTy(suc l)
  MCType l ->
    return $
      WfComputation
        { ctm = preTm,
          effOf = mempty,
          ctyOf = MVCType (l + 1)
        }
  _ -> throwError $ CannotInferComputation preTm

inferValueTy :: Context -> MetaTerm -> TypeCheckResult (WfValue, Int)
inferValueTy ctx preVTy = do
  vTy <- inferValue ctx preVTy
  case vtyOf vTy of
    MVVType l -> return (vTy, l)
    _ -> throwError $ ExpectedToBeValueTy preVTy

inferComputationTy :: Context -> MetaTerm -> TypeCheckResult (WfComputation, Int)
inferComputationTy ctx preCTy = do
  cTy <- inferComputation ctx preCTy
  case ctyOf cTy of
    MVVType l -> return (cTy, l)
    _ -> throwError $ ExpectedToBeComputationTy preCTy

checkValue :: Context -> MetaTerm -> MetaType -> TypeCheckResult WfValue
checkValue ctx preTm vTy = case (preTm, vTy) of
  (MThunk c, MVU e cTy) -> do
    -- Γ |- c <: e' ! C
    cTm <- checkComputation ctx c cTy

    -- for any e >= e', Γ |- thunk c <: U(e, C)
    if (e `gte` effOf cTm) == Just True then
      return $
        WfValue
          { vtm = MThunk (ctm cTm),
            vtyOf = MVU e (ctyOf cTm)
          }
    else
      throwError $ ComputationEffErr c (effOf cTm) e
  _ -> do
    vTm <- inferValue ctx preTm
    conv <- valueConversionCheck (ctxLvl ctx) (vtyOf vTm) vTy
    if conv then
      return $
        WfValue
          { vtm = vtm vTm,
            vtyOf = vTy
          }
    else
      throwError $ ValueUnify preTm (vtyOf vTm) vTy

checkComputation :: Context -> MetaTerm -> MetaType -> TypeCheckResult WfComputation
checkComputation ctx preTm cTy = case (preTm, cTy) of
  (MLam body, MVPi dom eff cls) -> do
    cod <- mapEvalResult $ evaluateMetaClosureFresh cls
    bodyTm <- checkComputation (ctx |:> dom) body cod
    return $
      WfComputation
        { ctm = ctm bodyTm,
          effOf = mempty,
          ctyOf = MVPi dom (eff \/ effOf bodyTm) cls
        }
  (MReturn v, MVF vTy) -> do
    vTm <- checkValue ctx v vTy
    return $
      WfComputation
        { ctm = vtm vTm,
          effOf = mempty,
          ctyOf = MVF (vtyOf vTm)
        }
  _ -> do
    cTm <- inferComputation ctx preTm
    conv <- computationConversionCheck (ctxLvl ctx) (ctyOf cTm) cTy
    if conv then
      return $
        WfComputation
          { ctm = ctm cTm,
            effOf = effOf cTm,
            ctyOf = cTy
          }
    else
      throwError $ ComputationUnify preTm (ctyOf cTm) cTy
