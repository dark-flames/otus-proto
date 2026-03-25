module Otus.Normalize.Subst (
  Subst (..),
  Substitutable (..),
  dropSb,
  extSb,
  extSb',
  composeSb,
  liftSb,
  liftNSb,
) where

import qualified Data.Sequence as Seq

import Otus.Ast
import Otus.Common

data SubstSeg
  = ObjSeg Term
  | MetaSeg MetaTerm
  | AmbiguousIdx IndexId

data Subst = Subst
  { baseDrop :: Int,
    substSeq :: Seq SubstSeg
  }

class SyntaxTerm tm where
  asSubstSeg :: tm -> SubstSeg

class Substitutable tm where
  subst :: tm -> Subst -> Maybe tm

  fSubst :: (Traversable f) => f tm -> Subst -> Maybe (f tm)
  fSubst l sb = traverse (`subst` sb) l

-- ΓΓ' => Γ
dropSb :: Int -> Subst
dropSb n = Subst n mempty

-- (Γ => Δ) -> A -> (Γ => Δ, A)
extSb :: (SyntaxTerm tm) => Subst -> tm -> Subst
extSb (Subst d s) tm = Subst d (s |> asSubstSeg tm)

extSb' :: (SyntaxTerm tm) => Subst -> Seq tm -> Subst
extSb' (Subst d s) tmSeq = Subst d (s >< fmap asSubstSeg tmSeq)

-- (Δ => ≡) -> (Γ => Δ) -> (Γ => ≡)
composeSb :: Subst -> Subst -> Maybe Subst
composeSb (Subst ld ls) r@(Subst rd rs) = do
  ls' <- fSubst ls r
  if ld <= size rs then
    return $ Subst rd (Seq.take (size rs - ld) rs >< ls')
  else
    return $ Subst (rd + size rs - ld) ls'

-- (δ: Γ => Δ) -> (Γ, A[δ] => Δ, A)
liftSb :: Subst -> Maybe Subst
liftSb = liftNSb 1

liftNSb :: Int -> Subst -> Maybe Subst
liftNSb n sb = do
  dSb <- composeSb sb (dropSb n)
  return $ extSb' dSb (fromList $ map IndexId (reverse [0 .. n]))

instance SyntaxTerm Term where
  asSubstSeg = ObjSeg

instance SyntaxTerm MetaTerm where
  asSubstSeg = MetaSeg

instance SyntaxTerm IndexId where
  asSubstSeg = AmbiguousIdx

instance Substitutable SubstSeg where
  subst :: SubstSeg -> Subst -> Maybe SubstSeg
  subst seg sb@(Subst d l) = case seg of
    ObjSeg t -> ObjSeg <$> subst t sb
    MetaSeg t -> MetaSeg <$> subst t sb
    AmbiguousIdx idx ->
      if unIndex idx < size l then
        l @? idx
      else
        return $ AmbiguousIdx $ shift (d - size l) idx

instance Substitutable MetaTerm where
  subst :: MetaTerm -> Subst -> Maybe MetaTerm
  subst tm sb@(Subst d l) = case tm of
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
    MLift tele -> MLift <$> subst tele sb
    MQuote record -> MQuote <$> subst record sb
    MDyn tele -> MDyn <$> subst tele sb
    MGuard cstr record -> do
      cstr' <- subst cstr sb
      sb' <- liftNSb (size cstr) sb
      record' <- subst record sb'
      return $ MGuard cstr' record'
    MExt prev n cstr record -> do
      prev' <- go prev
      sb' <- liftNSb n sb
      cstr' <- subst cstr sb'
      sb'' <- liftNSb (size cstr') sb'
      record' <- subst record sb''
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
    where
      go :: (Substitutable t) => t -> Maybe t
      go t = subst t sb

      goLift :: (Substitutable t) => t -> Maybe t
      goLift t = liftSb sb >>= subst t

instance Substitutable Term where
  subst :: Term -> Subst -> Maybe Term
  subst tm sb@(Subst d l) = case tm of
    Var idx ->
      if unIndex idx < size l then case l @? idx of
        Just (ObjSeg t) -> return t
        Just (AmbiguousIdx idx') -> return $ Var idx'
        _ -> Nothing
      else
        return $ Var $ shift (d - size l) idx -- idx - size l + d
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
    _ -> undefined
    where
      go :: (Substitutable t) => t -> Maybe t
      go t = subst t sb

      goLift :: (Substitutable t) => t -> Maybe t
      goLift t = liftSb sb >>= subst t

      goLiftN :: (Substitutable t) => Int -> t -> Maybe t
      goLiftN n t = liftNSb n sb >>= subst t

instance Substitutable Telescope where
  subst :: Telescope -> Subst -> Maybe Telescope
  subst (TeleSeq raw) sb = TeleSeq <$> go sb raw
    where
      go sb' = \case
        Empty -> return Empty
        ty :<| rst -> do
          ty' <- subst ty sb'
          sb'' <- liftSb sb'
          rst' <- go sb'' rst
          return $ ty' :<| rst'

instance Substitutable Constraint where
  subst :: Constraint -> Subst -> Maybe Constraint
  subst c sb = fst <$> go c
    where
      go = \case
        CstrEmpty -> return (CstrEmpty, sb)
        CstrDef prev ty -> do
          (prev', sb') <- go prev
          ty' <- subst ty sb'
          sb'' <- liftSb sb'
          return (CstrDef prev' ty', sb'')
        CstrTmEq prev tele lhs rhs ty -> do
          (prev', sb') <- go prev
          tele' <- subst tele sb'
          localSb <- liftNSb (size tele') sb
          lhs' <- subst lhs localSb
          rhs' <- subst rhs localSb
          ty' <- subst ty localSb
          sb'' <- liftSb sb'
          return (CstrTmEq prev' tele' lhs' rhs' ty', sb'')

instance Substitutable Record where
  subst :: Record -> Subst -> Maybe Record
  subst (RecordSeq raw) sb = RecordSeq <$> fSubst raw sb
