module Otus.Common.Seq (
  Seq (..),
  Sequence (..),
  SeqIndex (..),
  SeqModify (..),
  asSeq,
) where

import Data.Foldable (foldlM)
import Data.Sequence (Seq (..))

import qualified Data.Sequence as Seq

-- Sized

-- Seq
class Sequence l where
  type Item l

  fromSeq :: Seq (Item l) -> l
  toSeq :: l -> Seq (Item l)

  fromList :: [Item l] -> l
  fromList = fromSeq . Seq.fromList

  empty :: l
  empty = fromSeq Seq.Empty

  singleton :: Item l -> l
  singleton = fromSeq . Seq.singleton

  size :: l -> Int
  size = Seq.length . toSeq

  (|>) :: l -> Item l -> l
  l |> a = fromSeq (toSeq l Seq.|> a)

  (<|) :: Item l -> l -> l
  a <| l = fromSeq (a Seq.<| toSeq l)

  (><) :: (Item r ~ Item l, Sequence r) => l -> r -> l
  l >< r = fromSeq $ toSeq l Seq.>< toSeq r

  (@?) :: (SeqIndex id) => l -> id -> Maybe (Item l)
  l @? idx = toSeq l Seq.!? intoLeftIndex l idx

  seqMap :: (Item t ~ a, Sequence t) => (Item l -> a) -> l -> t
  seqMap f l = fromSeq $ fmap f (toSeq l)

  seqMapM :: (Item t ~ a, Monad m, Sequence t) => (Item l -> m a) -> l -> m t
  seqMapM f l = fromSeq <$> mapM f (toSeq l)

  seqFoldlM :: (Monad m) => (a -> Item l -> m a) -> a -> l -> m a
  seqFoldlM f s l = foldlM f s (toSeq l)

instance Sequence (Seq a) where
  type Item (Seq a) = a
  toSeq = id
  fromSeq = id

-- Index
class SeqIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  intoLeftIndex :: (Sequence a) => a -> id -> Int
  intoRightIndex :: (Sequence a) => a -> id -> Int

instance SeqIndex Int where
  shift = (+)
  sub = (-)
  intoLeftIndex = const id
  intoRightIndex l i = size l - i - 1

-- modify
class (Sequence l) => SeqModify l where
  adjust :: (Item l -> Item l) -> Int -> l -> l
  update :: Int -> Item l -> l -> l

instance SeqModify (Seq a) where
  adjust = Seq.adjust
  update = Seq.update

asSeq :: [a] -> Seq a
asSeq = fromList
