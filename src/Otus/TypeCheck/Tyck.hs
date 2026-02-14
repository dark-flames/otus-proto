module Otus.TypeCheck.Tyck (
  infer,
  inferTy,
  check,
  inferValue,
  inferComputation,
  inferValueTy,
  inferComputationTy,
  inferValueTy',
  inferComputationTy',
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

-- Object Language Type Checking
inferTy :: Context -> Term -> TypeCheckResult Type
inferTy ctx preTy = do
  (_, ty, _) <- inferTy' ctx preTy
  return ty

inferTy' :: Context -> Term -> TypeCheckResult (WfTerm, Type, Int)
inferTy' ctx preTy = do
  tyTm <- infer ctx preTy
  case tyOf tyTm of
    VType l -> do
      ty <- doEval (tm tyTm) (ctxEnv ctx)
      return (tyTm, ty, l)
    _ -> throwError $ CannotCheckAsType preTy

inferTelescope :: Context -> Telescope -> TypeCheckResult (Telescope, Int)
inferTelescope _ Empty = return (Empty, 0)
inferTelescope ctx (preTy :<| rest) = do
  (tyTm, ty, l) <- inferTy' ctx preTy
  (restTm, restL) <- inferTelescope (ctx |:> ty) rest
  return (tm tyTm :<| restTm, max l restL)

checkRecord :: Context -> Record -> VTelescope -> TypeCheckResult Record
checkRecord ctx preRecord tele = case (preRecord, tele) of
  (Empty, VTNil) -> return Empty
  (preTm :<| restPR, VTCons ty cls) -> do
    t <- check ctx preTm ty
    v <- doEval (tm t) (ctxEnv ctx)
    restTele <- doEvalTeleClosure v cls
    restR <- checkRecord (ctx |:> ty) restPR restTele
    return $ tm t :<| restR
  _ -> do
    teleTm <- doReadbackTele (ctxLvl ctx) tele
    throwError $ CannotCheckRecord preRecord teleTm

infer :: Context -> Term -> TypeCheckResult WfTerm
infer ctx = \case
  Var idx -> case ctx @? idx of
    Just (ObjTy ty) ->
      return $
        WfTerm
          { tm = Var idx,
            tyOf = ty
          }
    Just (MetaTy _) -> throwError CannotInferMetaAsObj
    _ -> throwError CannotInferIndex
  TyAnnotation preTm preTy -> do
    ty <- inferTy ctx preTy
    check ctx preTm ty
  Pi dom cod -> do
    (domTm, domTy, domL) <- inferTy' ctx dom
    (codTm, _, codL) <- inferTy' (ctx |:> domTy) cod
    return
      WfTerm
        { tm = Pi (tm domTm) (tm codTm),
          tyOf = VType $ max domL codL
        }
  Lam (Just preTy) body -> do
    (tyTm, ty, _) <- inferTy' ctx preTy
    bodyTm <- infer (ctx |:> ty) body
    codTyTm <- doReadback (incrLvl $ ctxLvl ctx) (tyOf bodyTm)
    return $
      WfTerm
        { tm = Lam (Just $ tm tyTm) (tm bodyTm),
          tyOf = VPi ty (Closure (ctxEnv ctx) codTyTm)
        }
  App f p -> do
    fTm <- infer ctx f
    case tyOf fTm of
      VPi dom cls -> do
        -- Γ |- p : A
        pTm <- check ctx p dom
        vP <- doEval (tm pTm) (ctxEnv ctx)
        -- Γ |- B[id, p] type
        res <- doEvalClosure vP cls
        -- Γ |- f p : B[id, p]
        return $
          WfTerm
            { tm = App (tm fTm) (tm pTm),
              tyOf = res
            }
      fTy -> do
        fTyTm <- doReadback (ctxLvl ctx) fTy
        throwError $ ExpectedToBeFn (tm fTm) fTyTm
  Record preTele -> do
    (teleTm, l) <- inferTelescope ctx preTele
    return $
      WfTerm
        { tm = Record teleTm,
          tyOf = VType l
        }
  First preTm -> do
    t <- infer ctx preTm
    case tyOf t of
      VRecord (VTCons headTy _) ->
        return $
          WfTerm
            { tm = First (tm t),
              tyOf = headTy
            }
      ty -> do
        tyTm <- doReadback (ctxLvl ctx) ty
        throwError $ ExpectedToBeNonEmptyRecord preTm tyTm
  Rest preTm -> do
    t <- infer ctx preTm
    case tyOf t of
      VRecord (VTCons _ cls) -> do
        val <- doEval (First preTm) (ctxEnv ctx)
        restTele <- doEvalTeleClosure val cls
        return $
          WfTerm
            { tm = Rest (tm t),
              tyOf = VRecord restTele
            }
      ty -> do
        tyTm <- doReadback (ctxLvl ctx) ty
        throwError $ ExpectedToBeNonEmptyRecord preTm tyTm
  Splicing preMeta -> do
    meta <- inferValue ctx preMeta
    case vtyOf meta of
      MVLift ty ->
        return $
          WfTerm
            { tm = Splicing (vtm meta),
              tyOf = VRecord ty
            }
      metaTy -> do
        metaTyTm <- doReadbackMeta (ctxLvl ctx) metaTy
        throwError $ CannotSplicing preMeta metaTyTm
  preTm -> throwError $ CannotInferTerm preTm

check :: Context -> Term -> Type -> TypeCheckResult WfTerm
check ctx preTm ty = case (preTm, ty) of
  (Lam oty body, VPi dom cls) -> do
    domTy <- case oty of
      Nothing -> return dom
      Just prePTy -> do
        pTy <- inferTy ctx prePTy
        c <- conv (ctxLvl ctx) pTy dom
        if c then
          return pTy
        else do
          let lvl = ctxLvl ctx
          domTm <- doReadback lvl dom
          throwError $ Unify preTm prePTy domTm

    cod <- doEvalClosureFresh cls
    bodyTm <- check (ctx |:> domTy) body cod
    return $
      WfTerm
        { tm = Lam oty $ tm bodyTm,
          tyOf = VPi dom cls
        }
  (List preRecord, VRecord tele) -> do
    record <- checkRecord ctx preRecord tele
    return $
      WfTerm
        { tm = List record,
          tyOf = VRecord tele
        }
  (Splicing preMeta, VRecord tele) -> do
    meta <- checkValue ctx preMeta (MVLift tele)
    return $
      WfTerm
        { tm = Splicing (vtm meta),
          tyOf = VRecord tele
        }
  _ -> do
    t <- infer ctx preTm
    c <- conv (ctxLvl ctx) (tyOf t) ty
    if c then
      return $
        WfTerm
          { tm = tm t,
            tyOf = ty
          }
    else do
      let lvl = ctxLvl ctx
      lTy <- doReadback lvl (tyOf t)
      rTy <- doReadback lvl ty
      throwError $ Unify preTm lTy rTy

-- Meta Language Type Checking
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
  MTyAnnotation preVTm preVTy -> do
    vTy <- inferValueTy ctx preVTy
    checkValue ctx preVTm vTy
  MU e preCTy -> do
    (cTyTm, _, l) <- inferComputationTy' ctx preCTy
    return $
      WfValue
        { vtm = MU e $ ctm cTyTm,
          vtyOf = MVVType l
        }
  MThunk c -> do
    -- Γ |- c <: e' ! C
    cTm <- inferComputation ctx c

    return $
      WfValue
        { vtm = MThunk (ctm cTm),
          vtyOf = MVU (effOf cTm) (ctyOf cTm)
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
  MTyAnnotation preCTm preCTy -> do
    cTy <- inferComputationTy ctx preCTy
    checkComputation ctx preCTm cTy
  MPi dom eff cod -> do
    -- Γ |- Dom vtype @ domL
    (domVTyTm, domVTy, domL) <- inferValueTy' ctx dom
    -- Γ, Dom |- Cod ctype @ codL ! e
    (codCTyTm, _, codL) <- inferComputationTy' (ctx |:> domVTy) cod

    -- Γ |- Pi(Dom, eff, Cod) ctype @ domL /\ codL ! e
    return $
      WfComputation
        { ctm = MPi (vtm domVTyTm) eff (ctm codCTyTm),
          effOf = effOf codCTyTm,
          ctyOf = MVCType (max domL codL)
        }
  MLam (Just preTy) body -> do
    (tyTm, ty, _) <- inferValueTy' ctx preTy
    bodyTm <- inferComputation (ctx |:> ty) body
    codTyTm <- doReadbackMeta (incrLvl $ ctxLvl ctx) (ctyOf bodyTm)
    return $
      WfComputation
        { ctm = MLam (Just $ vtm tyTm) (ctm bodyTm),
          effOf = mempty,
          ctyOf = MVPi ty (effOf bodyTm) (Closure (ctxEnv ctx) codTyTm)
        }
  MApp f p -> do
    -- Γ |- f :> e ! Pi A e' B
    fTm <- inferComputation ctx f
    let e = effOf fTm
    case ctyOf fTm of
      MVPi dom e' cls -> do
        -- Γ |- p<: A
        pTm <- checkValue ctx p dom
        vP <- doEvalValue (vtm pTm) (ctxEnv ctx)
        -- Γ |- B[id, p] ctype
        res <- doEvalMetaClosure vP cls
        -- Γ |- f p :> e \/ e' ! B[id, p]
        return $
          WfComputation
            { ctm = MApp (ctm fTm) (vtm pTm),
              effOf = e \/ e',
              ctyOf = res
            }
      fTy -> do
        fTyTm <- doReadbackMeta (ctxLvl ctx) fTy
        throwError $ ExpectedToBeMetaFn preTm fTyTm
  MF a -> do
    -- Γ |- A vtype @ l
    (aTm, _, l) <- inferValueTy' ctx a
    -- Γ |- F(A): empty ! CTy l
    return $
      WfComputation
        { ctm = MF (vtm aTm),
          effOf = mempty,
          ctyOf = MVCType l
        }
  MReturn v -> do
    vTm <- inferValue ctx v
    return $
      WfComputation
        { ctm = MReturn $ vtm vTm,
          effOf = mempty,
          ctyOf = MVF (vtyOf vTm)
        }
  MLetIn prev bind bindTy -> do
    -- Γ |- prev :> e ! F(A)
    prevTm <- inferComputation ctx prev
    let e = effOf prevTm
    case ctyOf prevTm of
      MVF vTy -> do
        let env = ctxEnv ctx
        -- Γ, U(e, F(A)) |- B ctype
        (kleisliExt, _, _) <- inferComputationTy' (ctx |:> MVU e (MVF vTy)) bindTy
        -- Γ, A |- drop(1), thunk(return v0) => Γ, U(e, F(A))
        let thunked = MVThunk (MVReturn $ mvvar $ envLevel env)
        -- Γ, A |- B[drop(1), thunk(return v0)] ctype
        bindCTy <-
          doEvalComputation (ctm kleisliExt) (env ||> thunked)
        -- Γ, A |- bind <: e' ! B[drop(1), thunk(return v0)]
        bindTm <- checkComputation (ctx |:> vTy) bind bindCTy
        let e' = effOf bindTm
        -- Γ |- id, thunk prev => Γ, U(e, F(A))
        vPrevTm <- doEvalComputation (ctm prevTm) env
        -- Γ |- B[id, thunk prev] ctype
        resultCTy <-
          doEvalComputation
            (ctm kleisliExt)
            (env ||> MVThunk vPrevTm)
        -- Γ |- let prev in bind :> e \/ e' ! B[thunk prev]
        return $
          WfComputation
            { ctm = MLetIn (ctm prevTm) (ctm bindTm) (ctm kleisliExt),
              effOf = e \/ e',
              ctyOf = resultCTy
            }
      cTy -> do
        t <- doReadbackMeta (ctxLvl ctx) cTy
        throwError $ CannotBindOn prev t
  MForce v -> do
    -- Γ |- v :> U e C
    vTm <- inferValue ctx v
    case vtyOf vTm of
      -- Γ |- force(v) :> e ! C
      MVU eff cTy ->
        return $
          WfComputation
            { ctm = MForce $ vtm vTm,
              effOf = eff,
              ctyOf = cTy
            }
      t -> do
        ty <- doReadbackMeta (ctxLvl ctx) t
        throwError $ CannotForce v ty
  -- Γ |- CTy(l) :> CTy(suc l)
  MCType l ->
    return $
      WfComputation
        { ctm = preTm,
          effOf = mempty,
          ctyOf = MVCType (l + 1)
        }
  _ -> throwError $ CannotInferComputation preTm

inferValueTy :: Context -> MetaTerm -> TypeCheckResult MetaType
inferValueTy ctx preVTy = do
  (_, ty, _) <- inferValueTy' ctx preVTy
  return ty

inferValueTy' :: Context -> MetaTerm -> TypeCheckResult (WfValue, MetaType, Int)
inferValueTy' ctx preVTy = do
  vTyTm <- inferValue ctx preVTy
  case vtyOf vTyTm of
    MVVType l -> do
      vTy <- doEvalValue (vtm vTyTm) (ctxEnv ctx)
      return (vTyTm, vTy, l)
    _ -> throwError $ ExpectedToBeValueTy preVTy

inferComputationTy :: Context -> MetaTerm -> TypeCheckResult MetaType
inferComputationTy ctx preCTy = do
  (_, ty, _) <- inferComputationTy' ctx preCTy
  return ty

inferComputationTy' :: Context -> MetaTerm -> TypeCheckResult (WfComputation, MetaType, Int)
inferComputationTy' ctx preCTy = do
  cTyTm <- inferComputation ctx preCTy
  case ctyOf cTyTm of
    MVCType l -> do
      cTy <- doEvalComputation (ctm cTyTm) (ctxEnv ctx)
      return (cTyTm, cTy, l)
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
    c <- valueConv (ctxLvl ctx) (vtyOf vTm) vTy
    if c then
      return $
        WfValue
          { vtm = vtm vTm,
            vtyOf = vTy
          }
    else do
      let lvl = ctxLvl ctx
      lTy <- doReadbackMeta lvl (vtyOf vTm)
      rTy <- doReadbackMeta lvl vTy
      throwError $ ValueUnify preTm lTy rTy

checkComputation :: Context -> MetaTerm -> MetaType -> TypeCheckResult WfComputation
checkComputation ctx preTm cTy = case (preTm, cTy) of
  (MLam oty body, MVPi dom eff cls) -> do
    domTy <- case oty of
      Nothing -> return dom
      Just prePTy -> do
        pTy <- inferValueTy ctx prePTy
        c <- computationConv (ctxLvl ctx) pTy dom
        if c then
          return pTy
        else do
          let lvl = ctxLvl ctx
          domTm <- doReadbackMeta lvl dom
          throwError $ ComputationUnify preTm prePTy domTm

    cod <- doEvalMetaClosureFresh cls
    bodyTm <- checkComputation (ctx |:> domTy) body cod
    return $
      WfComputation
        { ctm = MLam oty $ ctm bodyTm,
          effOf = mempty,
          ctyOf = MVPi dom (eff \/ effOf bodyTm) cls
        }
  (MReturn v, MVF vTy) -> do
    vTm <- checkValue ctx v vTy
    return $
      WfComputation
        { ctm = MReturn $ vtm vTm,
          effOf = mempty,
          ctyOf = MVF (vtyOf vTm)
        }
  (MTrigger e, _) -> do
    -- Γ |- trigger(e, C) : {e} ! C
    return $
      WfComputation
        { ctm = MTrigger e,
          effOf = singletonEff e,
          ctyOf = cTy
        }
  _ -> do
    cTm <- inferComputation ctx preTm
    c <- computationConv (ctxLvl ctx) (ctyOf cTm) cTy
    if c then
      return $
        WfComputation
          { ctm = ctm cTm,
            effOf = effOf cTm,
            ctyOf = cTy
          }
    else do
      let lvl = ctxLvl ctx
      lTy <- doReadbackMeta lvl (ctyOf cTm)
      rTy <- doReadbackMeta lvl cTy
      throwError $ ComputationUnify preTm lTy rTy
