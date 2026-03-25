module Otus.Ast.Term (
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

import Otus.Ast.Effect
import Otus.Ast.Id
import Otus.Common

newtype Telescope = TeleSeq
  { unTele :: Seq Term
  }
  deriving (Eq, Show)

newtype Record = RecordSeq
  { unRecord :: Seq Term
  }
  deriving (Eq, Show)

newtype Sequence = Sequence
  { unSeq :: Seq Term
  }
  deriving (Eq, Show)

data Constraint
  = CstrEmpty
  | CstrTmEq Constraint Telescope Term Term Term
  | CstrDef Constraint Term
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq)

instance Sized Telescope where
  size = size . unTele

instance Sized Record where
  size = size . unRecord

instance Sized Constraint where
  size CstrEmpty = 0
  size (CstrDef prev _) = size prev + 1
  size (CstrTmEq prev _ _ _ _) = size prev + 1

instance Pretty Telescope where
  pretty = pretty . unTele

instance Pretty Record where
  pretty = pretty . unRecord

instance Pretty Sequence where
  pretty = pretty . unSeq

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
