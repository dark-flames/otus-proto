module Otus.Normalize.Subst (
  ReduceExplicitSubst (..),
) where

import Otus.Ast.Id (unIndex)
import Otus.Ast.Term
import Otus.Common

class ReduceExplicitSubst tm where
  performSb :: tm -> Subst -> Maybe tm
  reduceSb :: tm -> Maybe tm

  fPerformSb :: (Traversable f) => f tm -> Subst -> Maybe (f tm)
  fPerformSb l sb = traverse (`performSb` sb) l

instance ReduceExplicitSubst Term where
  performSb tm (Subst 0 Empty) = return tm
  performSb tm sb@(Subst d l) = case tm of
    Var idx ->
      if unIndex idx < size l then case l @? idx of
        Just (ObjSeg t) -> return t
        Just (AmbiguousIdx idx') -> return $ Var idx'
        _ -> Nothing
      else
        return $ Var $ shift (d - size l) idx
    TyAnnotation t ty -> do
      t' <- go t
      ty' <- goLift ty
      return $ TyAnnotation t' ty'
    Pi dom cod -> do
      dom' <- go dom
      cod' <- go cod
      return $ Pi dom' cod'
    Lam ty body -> do
      ty' <- traverse go ty
      body' <- goLift body
      return $ Lam ty' body'
    App f p -> do
      f' <- go f
      p' <- go p
      return $ App f' p'
    Record tele -> Record <$> go tele
    List record -> List <$> go record
    First t -> First <$> go t
    Rest t -> Rest <$> go t
    Id ty lhs rhs -> do
      ty' <- go ty
      lhs' <- go lhs
      rhs' <- go rhs
      return $ Id ty' lhs' rhs'
    Refl -> return Refl
    J prop proof path -> do
      prop' <- goLiftN 2 prop
      proof' <- go proof
      path' <- go path
      return $ J prop' proof' path'
    Splicing mt -> Splicing <$> go mt
    Type i -> return $ Type i
    Substituted t innerSb -> do
      composed <- composeSb sb innerSb
      performSb t composed
    where
      go :: (ReduceExplicitSubst t) => t -> Maybe t
      go t = performSb t sb

      goLift :: (ReduceExplicitSubst t) => t -> Maybe t
      goLift t = liftSb sb >>= performSb t

      goLiftN :: (ReduceExplicitSubst t) => Int -> t -> Maybe t
      goLiftN n t = liftNSb n sb >>= performSb t

  reduceSb tm = case tm of
    Substituted t sb -> performSb t sb >>= reduceSb
    Var idx -> return $ Var idx
    TyAnnotation t ty -> TyAnnotation <$> reduceSb t <*> reduceSb ty
    Pi dom cod -> Pi <$> reduceSb dom <*> reduceSb cod
    Lam ty body -> Lam <$> traverse reduceSb ty <*> reduceSb body
    App f p -> App <$> reduceSb f <*> reduceSb p
    Record tele -> Record <$> reduceSb tele
    List record -> List <$> reduceSb record
    First t -> First <$> reduceSb t
    Rest t -> Rest <$> reduceSb t
    Id ty lhs rhs -> Id <$> reduceSb ty <*> reduceSb lhs <*> reduceSb rhs
    Refl -> return Refl
    J prop proof path -> J <$> reduceSb prop <*> reduceSb proof <*> reduceSb path
    Splicing mt -> Splicing <$> reduceSb mt
    Type i -> return $ Type i

instance ReduceExplicitSubst Telescope where
  performSb t (Subst 0 Empty) = return t
  performSb (TeleSeq raw) sb = TeleSeq <$> go sb raw
    where
      go sb' = \case
        Empty -> return Empty
        ty :<| rst -> do
          ty' <- performSb ty sb'
          sb'' <- liftSb sb'
          rst' <- go sb'' rst
          return $ ty' :<| rst'
  reduceSb (TeleSeq raw) = TeleSeq <$> traverse reduceSb raw

instance ReduceExplicitSubst Record where
  performSb r (Subst 0 Empty) = return r
  performSb (RecordSeq raw) sb = RecordSeq <$> fPerformSb raw sb
  reduceSb (RecordSeq raw) = RecordSeq <$> traverse reduceSb raw

instance ReduceExplicitSubst Sequence where
  performSb s (Subst 0 Empty) = return s
  performSb (Sequence raw) sb = Sequence <$> fPerformSb raw sb
  reduceSb (Sequence raw) = Sequence <$> traverse reduceSb raw

instance ReduceExplicitSubst Constraint where
  performSb c (Subst 0 Empty) = return c
  performSb c sb = fst <$> go c
    where
      go = \case
        CstrEmpty -> return (CstrEmpty, sb)
        CstrDef prev ty -> do
          (prev', sb') <- go prev
          ty' <- performSb ty sb'
          sb'' <- liftSb sb'
          return (CstrDef prev' ty', sb'')
        CstrTmEq prev tele lhs rhs ty -> do
          (prev', sb') <- go prev
          tele' <- performSb tele sb'
          localSb <- liftNSb (size tele') sb
          lhs' <- performSb lhs localSb
          rhs' <- performSb rhs localSb
          ty' <- performSb ty localSb
          sb'' <- liftSb sb'
          return (CstrTmEq prev' tele' lhs' rhs' ty', sb'')
  reduceSb = \case
    CstrEmpty -> return CstrEmpty
    CstrDef prev ty -> CstrDef <$> reduceSb prev <*> reduceSb ty
    CstrTmEq prev tele lhs rhs ty ->
      CstrTmEq <$> reduceSb prev <*> reduceSb tele <*> reduceSb lhs <*> reduceSb rhs <*> reduceSb ty

instance ReduceExplicitSubst MetaTerm where
  performSb tm (Subst 0 Empty) = return tm
  performSb tm sb@(Subst d l) = case tm of
    MVar idx ->
      if unIndex idx < size l then case l @? idx of
        Just (MetaSeg t) -> return t
        Just (AmbiguousIdx idx') -> return $ MVar idx'
        _ -> Nothing
      else
        return $ MVar $ shift (d - size l) idx
    MTyAnnotation t ty -> do
      t' <- go t
      ty' <- go ty
      return $ MTyAnnotation t' ty'
    MU eff cTy -> MU eff <$> go cTy
    MThunk c -> MThunk <$> go c
    MVType i -> return $ MVType i
    MLift tele -> MLift <$> performSb tele sb
    MQuote record -> MQuote <$> performSb record sb
    MDyn tele -> MDyn <$> performSb tele sb
    MGuard cstr record -> do
      cstr' <- performSb cstr sb
      sb' <- liftNSb (size cstr) sb
      record' <- performSb record sb'
      return $ MGuard cstr' record'
    MExt prev n cstr record -> do
      prev' <- go prev
      sb' <- liftNSb n sb
      cstr' <- performSb cstr sb'
      sb'' <- liftNSb (size cstr') sb'
      record' <- performSb record sb''
      return $ MExt prev' n cstr' record'
    MPi dom eff cod -> do
      dom' <- go dom
      cod' <- goLift cod
      return $ MPi dom' eff cod'
    MLam oTy body -> do
      oTy' <- traverse go oTy
      body' <- goLift body
      return $ MLam oTy' body'
    MApp f p -> MApp <$> go f <*> go p
    MF ty -> MF <$> go ty
    MReturn t -> MReturn <$> go t
    MTrigger e -> return $ MTrigger e
    MLetIn prev body bindTy -> do
      prev' <- go prev
      body' <- goLift body
      bindTy' <- goLift bindTy
      return $ MLetIn prev' body' bindTy'
    MForce t -> MForce <$> go t
    MCType i -> return $ MCType i
    MSolve t -> MSolve <$> go t
    MSubstituted t innerSb -> do
      composed <- composeSb sb innerSb
      performSb t composed
    where
      go :: (ReduceExplicitSubst t) => t -> Maybe t
      go t = performSb t sb

      goLift :: (ReduceExplicitSubst t) => t -> Maybe t
      goLift t = liftSb sb >>= performSb t

  reduceSb tm = case tm of
    MSubstituted t sb -> performSb t sb >>= reduceSb
    MVar idx -> return $ MVar idx
    MTyAnnotation t ty -> MTyAnnotation <$> reduceSb t <*> reduceSb ty
    MU eff cTy -> MU eff <$> reduceSb cTy
    MThunk c -> MThunk <$> reduceSb c
    MVType i -> return $ MVType i
    MLift tele -> MLift <$> reduceSb tele
    MQuote record -> MQuote <$> reduceSb record
    MDyn tele -> MDyn <$> reduceSb tele
    MGuard cstr record -> MGuard <$> reduceSb cstr <*> reduceSb record
    MExt prev n cstr record -> MExt <$> reduceSb prev <*> pure n <*> reduceSb cstr <*> reduceSb record
    MPi dom eff cod -> MPi <$> reduceSb dom <*> pure eff <*> reduceSb cod
    MLam oTy body -> MLam <$> traverse reduceSb oTy <*> reduceSb body
    MApp f p -> MApp <$> reduceSb f <*> reduceSb p
    MF ty -> MF <$> reduceSb ty
    MReturn t -> MReturn <$> reduceSb t
    MTrigger e -> return $ MTrigger e
    MLetIn prev body bindTy -> MLetIn <$> reduceSb prev <*> reduceSb body <*> reduceSb bindTy
    MForce t -> MForce <$> reduceSb t
    MCType i -> return $ MCType i
    MSolve t -> MSolve <$> reduceSb t
