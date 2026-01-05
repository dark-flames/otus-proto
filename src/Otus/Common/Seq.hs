module Otus.Common.Seq (
  Seq (..),
  Sequence (..),
  SeqIndex (..),
  SeqModify (..),
  SeqSize (..),
  asSeq,
) where

import Data.Foldable (foldlM)
import Data.Sequence (Seq (..))

import qualified Data.Sequence as Seq

-- Sized

-- Seq
class (SeqSize l) => Sequence l where
  type Item l

  fromSeq :: Seq (Item l) -> l
  toSeq :: l -> Seq (Item l)

  fromList :: [Item l] -> l
  fromList = fromSeq . Seq.fromList

  empty :: l
  empty = fromSeq Seq.Empty

  singleton :: Item l -> l
  singleton = fromSeq . Seq.singleton

  (|>) :: l -> Item l -> l
  l |> a = fromSeq (toSeq l Seq.|> a)

  (<|) :: Item l -> l -> l
  a <| l = fromSeq (a Seq.<| toSeq l)

  (><) :: (Item r ~ Item l, Sequence r) => l -> r -> l
  l >< r = fromSeq $ toSeq l Seq.>< toSeq r

  (@?) :: (SeqIndex id) => l -> id -> Maybe (Item l)
  l @? idx = toSeq l Seq.!? intoLeftIndex l idx

  index :: l -> Int -> Item l
  index l = Seq.index (toSeq l)

  mapWithIndex :: (Sequence t) => (Int -> Item l -> Item t) -> l -> t
  mapWithIndex f l = fromSeq $ mapWithIndex f (toSeq l)

  seqMap :: (Sequence t) => (Item l -> Item t) -> l -> t
  seqMap f l = fromSeq $ fmap f (toSeq l)

  seqMapM :: (Monad m, Sequence t) => (Item l -> m (Item t)) -> l -> m t
  seqMapM f l = fromSeq <$> mapM f (toSeq l)

  seqFoldlM :: (Monad m) => (a -> Item l -> m a) -> a -> l -> m a
  seqFoldlM f s l = foldlM f s (toSeq l)

  seqMAppendM :: (Monad m, Monoid a) => (Item l -> m a) -> l -> m a
  seqMAppendM f = seqFoldlM (\c l -> mappend c <$> f l) mempty

  cycleTaking :: Int -> [Item l] -> l
  cycleTaking s = fromSeq . Seq.cycleTaking s . fromList

instance SeqSize (Seq a) where
  size = Seq.length

instance Sequence (Seq a) where
  type Item (Seq a) = a
  toSeq = id
  fromSeq = id

-- Index
class SeqIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  intoLeftIndex :: (SeqSize l) => l -> id -> Int
  intoRightIndex :: (SeqSize l) => l -> id -> Int

instance SeqIndex Int where
  shift = (+)
  sub = (-)
  intoLeftIndex = const id
  intoRightIndex l i = size l - i - 1

-- modify
class (Sequence l) => SeqModify l where
  adjust :: (SeqIndex id) => (Item l -> Item l) -> id -> l -> l
  update :: (SeqIndex id) => id -> Item l -> l -> l

instance SeqModify (Seq a) where
  adjust f idx l = Seq.adjust f (intoLeftIndex l idx) l
  update idx v l = Seq.update (intoLeftIndex l idx) v l

asSeq :: [a] -> Seq a
asSeq = fromList

class SeqSize l where
  size :: l -> Int

instance SeqSize Int where
  size = id
