module Otus.TypeCheck.Tyck (
  TypeCheck (..),
) where

import Control.Monad.Error.Class (MonadError (throwError))

import Otus.Ast
import Otus.Common
import Otus.Normalize
import Otus.TypeCheck.Context
import Otus.TypeCheck.Conv
import Otus.TypeCheck.Error
import Otus.TypeCheck.Judgement

class (Pretty tm) => TypeCheck tm where
  type Ty tm
  type TmJdg tm

  infer :: Context -> tm -> TypeCheckResult (TmJdg tm)

  check :: Context -> tm -> Ty tm -> TypeCheckResult (TmJdg tm)

  inferTy
    :: (EvalRes tm ~ Ty tm, Evaluatable tm, Tm (TmJdg tm) ~ tm, TmTy (TmJdg tm) ~ EvalRes tm)
    => Context -> tm -> TypeCheckResult (Ty tm)
  inferTy ctx preTy = do
    (_, ty, _) <- inferTy' ctx preTy
    return ty

  inferTy'
    :: (EvalRes tm ~ Ty tm, Evaluatable tm, Tm (TmJdg tm) ~ tm, TmTy (TmJdg tm) ~ EvalRes tm)
    => Context -> tm -> TypeCheckResult (TmJdg tm, Ty tm, Int)

-- Object Language Type Checking
inferTelescope :: Context -> Telescope -> TypeCheckResult (Telescope, Int)
inferTelescope c t = mapFst TeleSeq <$> go c (unTele t)
  where
    go _ Empty = return (Empty, 0)
    go ctx (preTy :<| rest) = do
      (tyTm, ty, l) <- inferTy' ctx preTy
      (restTm, restL) <- go (ctx |:> ty) rest
      return (tmOf tyTm :<| restTm, max l restL)

checkRecord :: Context -> Record -> VTelescope -> TypeCheckResult Record
checkRecord c r vt = RecordSeq <$> go c (unRecord r) vt
  where
    go ctx preRecord tele = case (preRecord, tele) of
      (Empty, VTNil) -> return Empty
      (preTm :<| restPR, VTCons ty cls) -> do
        t <- check ctx preTm ty
        v <- doEval ctx (tmOf t)
        restTele <- doEvalClosure v cls
        restR <- go (ctx |:> ty) restPR restTele
        return $ tmOf t :<| restR
      _ -> do
        teleTm <- doQuote ctx tele
        throwError $ CannotCheckRecord (RecordSeq preRecord) teleTm

instance TypeCheck Term where
  type Ty Term = Type
  type TmJdg Term = WfTerm

  infer ctx = \case
    Var idx -> case ctx @? idx of
      Just (ObjTy ty) ->
        return $
          WfTerm
            { jTm = Var idx,
              jTy = ty
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
          { jTm = Pi (tmOf domTm) (tmOf codTm),
            jTy = VType $ max domL codL
          }
    Lam (Just preTy) body -> do
      (tyTm, ty, _) <- inferTy' ctx preTy
      bodyTm <- infer (ctx |:> ty) body
      codTyTm <- doQuote (ctx |:> ty) (tyOf bodyTm)
      return $
        WfTerm
          { jTm = Lam (Just $ tmOf tyTm) (tmOf bodyTm),
            jTy = VPi ty (Closure (ctxEnv ctx) codTyTm)
          }
    App f p -> do
      fTm <- infer ctx f
      case tyOf fTm of
        VPi dom cls -> do
          -- Γ |- p : A
          pTm <- check ctx p dom
          vP <- doEval ctx (tmOf pTm)
          -- Γ |- B[id, p] type
          res <- doEvalClosure vP cls
          -- Γ |- f p : B[id, p]
          return $
            WfTerm
              { jTm = App (tmOf fTm) (tmOf pTm),
                jTy = res
              }
        fTy -> do
          fTyTm <- doQuote ctx fTy
          throwError $ ExpectedToBeFn (tmOf fTm) fTyTm
    Record preTele -> do
      (teleTm, l) <- inferTelescope ctx preTele
      return $
        WfTerm
          { jTm = Record teleTm,
            jTy = VType l
          }
    First preTm -> do
      t <- infer ctx preTm
      case tyOf t of
        VRecord (VTCons headTy _) ->
          return $
            WfTerm
              { jTm = First (tmOf t),
                jTy = headTy
              }
        ty -> do
          tyTm <- doQuote ctx ty
          throwError $ ExpectedToBeNonEmptyRecord preTm tyTm
    Rest preTm -> do
      t <- infer ctx preTm
      case tyOf t of
        VRecord (VTCons _ cls) -> do
          val <- doEval ctx (First preTm)
          restTele <- doEvalClosure val cls
          return $
            WfTerm
              { jTm = Rest (tmOf t),
                jTy = VRecord restTele
              }
        ty -> do
          tyTm <- doQuote ctx ty
          throwError $ ExpectedToBeNonEmptyRecord preTm tyTm
    Splicing preMeta -> do
      meta <- infer ctx preMeta
      case tyOf meta of
        MVLift ty ->
          return $
            WfTerm
              { jTm = Splicing (tmOf meta),
                jTy = VRecord ty
              }
        metaTy -> do
          metaTyTm <- doQuote ctx metaTy
          throwError $ CannotSplicing preMeta metaTyTm
    preTm -> throwError $ CannotInferTerm preTm

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
            domTm <- doQuote ctx dom
            throwError $ Unify preTm prePTy domTm

      cod <- doEvalClosureFresh cls
      bodyTm <- check (ctx |:> domTy) body cod
      return $
        WfTerm
          { jTm = Lam oty $ tmOf bodyTm,
            jTy = VPi dom cls
          }
    (List preRecord, VRecord tele) -> do
      record <- checkRecord ctx preRecord tele
      return $
        WfTerm
          { jTm = List record,
            jTy = VRecord tele
          }
    (Splicing preMeta, VRecord tele) -> do
      meta <- check ctx preMeta (MVLift tele)
      return $
        WfTerm
          { jTm = Splicing (tmOf meta),
            jTy = VRecord tele
          }
    _ -> do
      t <- infer ctx preTm
      c <- conv (ctxLvl ctx) (tyOf t) ty
      if c then
        return $
          WfTerm
            { jTm = tmOf t,
              jTy = ty
            }
      else do
        lTy <- doQuote ctx (tyOf t)
        rTy <- doQuote ctx ty
        throwError $ Unify preTm lTy rTy
  inferTy' ctx preTy = do
    tyTm <- infer ctx preTy
    case tyOf tyTm of
      VType l -> do
        t <- doEval ctx (tmOf tyTm)
        return (tyTm, t, l)
      _ -> throwError $ CannotCheckAsType (pretty preTy)

-- Meta Language Type Checking
instance TypeCheck MetaTerm where
  type Ty MetaTerm = MetaType
  type TmJdg MetaTerm = WfMetaTerm

  infer ctx = \case
    MVar idx -> case ctx @? idx of
      Just (MetaTy ty) ->
        return $ wfMetaValue (MVar idx) ty
      Just (ObjTy _) -> throwError CannotInferObjAsMeta
      _ -> throwError CannotInferIndex
    MTyAnnotation preTm preTy -> do
      ty <- inferTy ctx preTy
      check ctx preTm ty
    MU e preCTy -> do
      (cTyTm, _, l) <- inferTy' ctx preCTy
      return $ wfMetaValue (MU e $ tmOf cTyTm) (MVVType l)
    MThunk c -> do
      -- Γ |- c <: e' ! C
      cTm <- infer ctx c
      return $ wfMetaValue (MThunk (tmOf cTm)) (MVU (effOf cTm) (tyOf cTm))
    MVType l ->
      return $ wfMetaValue (MVType l) (MVVType (l + 1))
    ---- Computation
    MPi dom eff cod -> do
      -- Γ |- Dom vtype @ domL
      (domTm, domTy, domL) <- inferTy' ctx dom
      -- Γ, Dom |- Cod ctype @ codL ! e
      (codTm, _, codL) <- inferTy' (ctx |:> domTy) cod

      -- Γ |- Pi(Dom, eff, Cod) ctype @ domL /\ codL ! e
      return $
        WfMetaTerm
          { jTm = MPi (tmOf domTm) eff (tmOf codTm),
            jEff = effOf codTm,
            jTy = MVCType (max domL codL)
          }
    MLam (Just preTy) body -> do
      (tyTm, ty, _) <- inferTy' ctx preTy
      bodyTm <- infer (ctx |:> ty) body
      codTyTm <- doQuote (ctx |:> ty) (tyOf bodyTm)
      return $
        WfMetaTerm
          { jTm = MLam (Just $ tmOf tyTm) (tmOf bodyTm),
            jEff = mempty,
            jTy = MVPi ty (effOf bodyTm) (Closure (ctxEnv ctx) codTyTm)
          }
    MApp f p -> do
      -- Γ |- f :> e ! Pi A e' B
      fTm <- infer ctx f
      let e = effOf fTm
      case tyOf fTm of
        MVPi dom e' cls -> do
          -- Γ |- p<: A
          pTm <- check ctx p dom
          vP <- doEval ctx (tmOf pTm)
          -- Γ |- B[id, p] ctype
          res <- doEvalClosure vP cls
          -- Γ |- f p :> e \/ e' ! B[id, p]
          return $
            WfMetaTerm
              { jTm = MApp (tmOf fTm) (tmOf pTm),
                jEff = e \/ e',
                jTy = res
              }
        fTy -> do
          fTyTm <- doQuote ctx fTy
          throwError $ ExpectedToBeMetaFn (MApp f p) fTyTm
    MF preTy -> do
      -- Γ |- A vtype @ l
      (tyTm, _, l) <- inferTy' ctx preTy
      -- Γ |- F(A): empty ! CTy l
      return $
        WfMetaTerm
          { jTm = MF (tmOf tyTm),
            jEff = mempty,
            jTy = MVCType l
          }
    MReturn v -> do
      vTm <- infer ctx v
      return $
        WfMetaTerm
          { jTm = MReturn $ tmOf vTm,
            jEff = mempty,
            jTy = MVF (tyOf vTm)
          }
    MLetIn prev bind preBindTy -> do
      -- Γ |- prev :> e ! F(A)
      prevTm <- infer ctx prev
      let e = effOf prevTm
      case tyOf prevTm of
        MVF vTy -> do
          let env = ctxEnv ctx
          -- Γ, U(e, F(A)) |- B ctype
          (bindTyTm, _, _) <- inferTy' (ctx |:> MVU e (MVF vTy)) preBindTy
          -- Γ, A |- drop(1), thunk(return v0) => Γ, U(e, F(A))
          let thunked = MVThunk (MVReturn $ mvvar $ envLevel env)
          -- Γ, A |- B[drop(1), thunk(return v0)] ctype
          bindCTy <-
            doEval' ctx [thunked] (tmOf bindTyTm)
          -- Γ, A |- bind <: e' ! B[drop(1), thunk(return v0)]
          bindTm <- check (ctx |:> vTy) bind bindCTy
          let e' = effOf bindTm
          -- Γ |- id, thunk prev => Γ, U(e, F(A))
          vPrevTm <- doEval ctx (tmOf prevTm)
          -- Γ |- B[id, thunk prev] ctype
          resultCTy <-
            doEval' ctx [MVThunk vPrevTm] (tmOf bindTyTm)
          -- Γ |- let prev in bind :> e \/ e' ! B[thunk prev]
          return $
            WfMetaTerm
              { jTm = MLetIn (tmOf prevTm) (tmOf bindTm) (tmOf bindTyTm),
                jEff = e \/ e',
                jTy = resultCTy
              }
        cTy -> do
          t <- doQuote ctx cTy
          throwError $ CannotBindOn prev t
    MForce v -> do
      -- Γ |- v :> U e C
      vTm <- infer ctx v
      case tyOf vTm of
        -- Γ |- force(v) :> e ! C
        MVU eff cTy ->
          return $
            WfMetaTerm
              { jTm = MForce $ tmOf vTm,
                jEff = eff,
                jTy = cTy
              }
        t -> do
          ty <- doQuote ctx t
          throwError $ CannotForce v ty
    -- Γ |- CTy(l) :> CTy(suc l)
    MCType l ->
      return $
        WfMetaTerm
          { jTm = MCType l,
            jEff = mempty,
            jTy = MVCType (l + 1)
          }
    preTm -> throwError $ CannotInferValue preTm

  check ctx preTm ty = case (preTm, ty) of
    (MThunk c, MVU e cTy) -> do
      -- Γ |- c <: e' ! C
      cTm <- check ctx c cTy

      -- for any e >= e', Γ |- thunk c <: U(e, C)
      if (e `gte` effOf cTm) == Just True then
        return $ wfMetaValue (MThunk (tmOf cTm)) (MVU e (tyOf cTm))
      else
        throwError $ ComputationEffErr c (effOf cTm) e
    ---- Computation
    (MLam oty body, MVPi dom eff cls) -> do
      domTy <- case oty of
        Nothing -> return dom
        Just prePTy -> do
          pTy <- inferTy ctx prePTy
          c <- conv (ctxLvl ctx) pTy dom
          if c then
            return pTy
          else do
            domTm <- doQuote ctx dom
            throwError $ ComputationUnify preTm prePTy domTm

      cod <- doEvalClosureFresh cls
      bodyTm <- check (ctx |:> domTy) body cod
      return $
        WfMetaTerm
          { jTm = MLam oty $ tmOf bodyTm,
            jEff = mempty,
            jTy = MVPi dom (eff \/ effOf bodyTm) cls
          }
    (MReturn v, MVF vty) -> do
      vTm <- check ctx v vty
      return $
        WfMetaTerm
          { jTm = MReturn $ tmOf vTm,
            jEff = mempty,
            jTy = MVF (tyOf vTm)
          }
    (MTrigger e, _) -> do
      -- Γ |- trigger(e, C) : {e} ! C
      return $
        WfMetaTerm
          { jTm = MTrigger e,
            jEff = singletonEff e,
            jTy = ty
          }
    _ -> do
      tm <- infer ctx preTm
      c <- conv (ctxLvl ctx) (tyOf tm) ty
      if c then
        return $
          WfMetaTerm
            { jTm = tmOf tm,
              jEff = effOf tm,
              jTy = ty
            }
      else do
        lTy <- doQuote ctx (tyOf tm)
        rTy <- doQuote ctx ty
        throwError $ ComputationUnify preTm lTy rTy

  inferTy' ctx preTy = do
    tyTm <- infer ctx preTy
    case tyOf tyTm of
      MVVType l -> do
        vTy <- doEval ctx (tmOf tyTm)
        return (tyTm, vTy, l)
      MVCType l -> do
        cTy <- doEval ctx (tmOf tyTm)
        return (tyTm, cTy, l)
      _ -> throwError $ ExpectedToBeMetaTy preTy
