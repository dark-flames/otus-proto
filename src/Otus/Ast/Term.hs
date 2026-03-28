module Otus.Ast.Term (
  -- Subst
  SubstSeg (..),
  Subst (..),
  SyntaxTerm (..),
  Substitutable (..),
  idSb,
  dropSb,
  extSb,
  extSb',
  composeSb,
  liftNSb,
  liftSb,
  -- Term
  Telescope (..),
  Record (..),
  Sequence (..),
  Constraint (..),
  Term (..),
  EffectSet,
  MetaTerm (..),
  piTele,
  lamN,
) where

import qualified Data.Sequence as Seq

import Otus.Ast.Effect
import Otus.Ast.Id
import Otus.Common

data SubstSeg
  = ObjSeg Term
  | MetaSeg MetaTerm
  | AmbiguousIdx IndexId
  deriving (Eq, Show)

data Subst = Subst
  { baseDrop :: Int,
    substSeq :: Seq SubstSeg
  }
  deriving (Eq, Show)

class SyntaxTerm tm where
  asSubstSeg :: tm -> SubstSeg

class Substitutable tm where
  subst :: tm -> Subst -> Maybe tm

  fSubst :: (Traversable f) => f tm -> Subst -> Maybe (f tm)
  fSubst l sb = traverse (`subst` sb) l

instance SyntaxTerm IndexId where
  asSubstSeg = AmbiguousIdx

instance Substitutable SubstSeg where
  subst :: SubstSeg -> Subst -> Maybe SubstSeg
  subst seg (Subst 0 Empty) = return seg
  subst seg sb@(Subst d l) = case seg of
    ObjSeg t -> ObjSeg <$> subst t sb
    MetaSeg t -> MetaSeg <$> subst t sb
    AmbiguousIdx idx ->
      if unIndex idx < size l then
        l @? idx
      else
        return $ AmbiguousIdx $ shift (d - size l) idx

-- ΓΓ' => Γ
idSb :: Subst
idSb = dropSb 0

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

-- Telescope
newtype Telescope = TeleSeq
  { unTele :: Seq Term
  }
  deriving (Eq, Show)

instance Sized Telescope where
  size = size . unTele

instance Pretty Telescope where
  pretty = pretty . unTele

instance Substitutable Telescope where
  subst :: Telescope -> Subst -> Maybe Telescope
  subst t (Subst 0 Empty) = return t
  subst (TeleSeq raw) sb = TeleSeq <$> go sb raw
    where
      go sb' = \case
        Empty -> return Empty
        ty :<| rst -> do
          ty' <- subst ty sb'
          sb'' <- liftSb sb'
          rst' <- go sb'' rst
          return $ ty' :<| rst'

-- Record
newtype Record = RecordSeq
  { unRecord :: Seq Term
  }
  deriving (Eq, Show)

instance Sized Record where
  size = size . unRecord

instance Pretty Record where
  pretty = pretty . unRecord

instance Substitutable Record where
  subst :: Record -> Subst -> Maybe Record
  subst r (Subst 0 Empty) = return r
  subst (RecordSeq raw) sb = RecordSeq <$> fSubst raw sb

-- Sequence
newtype Sequence = Sequence
  { unSeq :: Seq Term
  }
  deriving (Eq, Show)

instance Pretty Sequence where
  pretty = pretty . unSeq

-- Constraint
data Constraint
  = CstrEmpty
  | CstrTmEq Constraint Telescope Term Term Term
  | CstrDef Constraint Term
  deriving (Eq, Show)

instance Sized Constraint where
  size CstrEmpty = 0
  size (CstrDef prev _) = size prev + 1
  size (CstrTmEq prev _ _ _ _) = size prev + 1

instance Substitutable Constraint where
  subst :: Constraint -> Subst -> Maybe Constraint
  subst c (Subst 0 Empty) = return c
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

-- Term
type OptionalTy = Maybe Term

data Term
  = Var IndexId
  | TyAnnotation Term Term
  | -- Pi type
    Pi Term Term
  | Lam OptionalTy Term
  | App Term Term
  | -- Record
    Record Telescope
  | List Record
  | First Term
  | Rest Term
  | Id Term Term Term
  | Refl
  | -- Γ |- p : Id(A, x, y)  Γ, A, Id(A, x, v0) |- B type   Γ |- e : B[x, refl]
    -- Γ |- J(B, e, p) : B[y, e]
    J Term Term Term
  | -- Embedding
    Splicing MetaTerm
  | -- Universe
    Type Int
  | Substituted Term Subst
  deriving (Eq, Show)

instance Pretty Term where
  pretty = \case
    Var i -> "%" ++ show (unIndex i)
    TyAnnotation t ty -> pretty t ++ ":: (" ++ pretty ty ++ ")"
    Pi dom cod -> "Π(" ++ pretty dom ++ ")." ++ pretty cod ++ ")"
    Lam oty body -> case oty of
      Nothing -> "λ. (" ++ pretty body ++ ")"
      Just t -> "λ(" ++ pretty t ++ "). (" ++ pretty body ++ ")"
    App f p -> pretty f ++ " ∘ " ++ pretty p
    Record tele -> "Record{" ++ pretty tele ++ "}"
    List record -> "[" ++ pretty record ++ "]"
    First t -> "fst(" ++ pretty t ++ ")"
    Rest t -> "rst(" ++ pretty t ++ ")"
    Id t l r -> "Id(" ++ pretty t ++ ", " ++ pretty l ++ ", " ++ pretty r ++ ")"
    Refl -> "refl"
    J fam p e -> "j(" ++ pretty fam ++ ", " ++ pretty p ++ ", " ++ pretty e ++ ")"
    Splicing t -> "<" ++ pretty t ++ ">"
    Type i -> "Ty(" ++ show i ++ ")"
    Substituted tm sb -> show tm ++ "[" ++ show sb ++ "]"

instance SyntaxTerm Term where
  asSubstSeg = ObjSeg

instance Substitutable Term where
  subst :: Term -> Subst -> Maybe Term
  subst tm (Subst 0 Empty) = return tm
  subst tm sb = return $ Substituted tm sb

-- Meta Term
type OptionalMetaTy = Maybe MetaTerm

data MetaTerm
  = -- Value
    MVar IndexId
  | MTyAnnotation MetaTerm MetaTerm
  | MU EffectSet MetaTerm
  | MThunk MetaTerm
  | MVType Int
  | ---- Embedding
    MLift Telescope
  | MQuote Record
  | MDyn Telescope
  | MGuard Constraint Record
  | MExt MetaTerm Int Constraint Record
  | -- Computation
    MPi MetaTerm EffectSet MetaTerm
  | MLam OptionalMetaTy MetaTerm
  | MApp MetaTerm MetaTerm
  | MF MetaTerm
  | MReturn MetaTerm
  | MTrigger Effect
  | MLetIn MetaTerm MetaTerm MetaTerm
  | MForce MetaTerm
  | MCType Int
  | MSolve MetaTerm
  | MSubstituted MetaTerm Subst
  deriving (Eq)

instance Pretty MetaTerm where
  pretty = \case
    MVar i -> "%" ++ show (unIndex i)
    MTyAnnotation t ty -> pretty t ++ ":: (" ++ pretty ty ++ ")"
    MU eff t -> "U(" ++ pretty eff ++ " ! " ++ pretty t ++ ")"
    MThunk t -> "[" ++ pretty t ++ "]"
    MVType i -> "VTy(" ++ show i ++ ")"
    MPi dom eff cod -> "Π(" ++ pretty dom ++ ")." ++ pretty eff ++ "!" ++ pretty cod ++ ")"
    MLam oty body -> case oty of
      Nothing -> "λ. (" ++ pretty body ++ ")"
      Just t -> "λ(" ++ pretty t ++ "). (" ++ pretty body ++ ")"
    MApp f p -> pretty f ++ " ∘ " ++ pretty p
    MF t -> "F(" ++ pretty t ++ ")"
    MReturn t -> "return(" ++ pretty t ++ ")"
    MTrigger eff -> "trigger(" ++ pretty eff ++ ")"
    MLetIn p b bTy -> "let (" ++ pretty p ++ ") in " ++ pretty b ++ " ::(" ++ pretty bTy ++ ")"
    MForce t -> "force(" ++ pretty t ++ ")"
    MCType i -> "CTy(" ++ show i ++ ")"
    _ -> "undefinded"

instance Show MetaTerm where
  show = pretty

instance SyntaxTerm MetaTerm where
  asSubstSeg = MetaSeg

instance Substitutable MetaTerm where
  subst :: MetaTerm -> Subst -> Maybe MetaTerm
  subst tm (Subst 0 Empty) = return tm
  subst tm sb = return $ MSubstituted tm sb

piTele :: Telescope -> Term -> Term
piTele tele cod = go $ unTele tele
  where
    go = \case
      Empty -> cod
      ty :<| rst -> Pi ty (go rst)

lamN :: Int -> Term -> Term
lamN = \case
  x | x > 0 -> Lam Nothing . lamN (x - 1)
  _ -> id
