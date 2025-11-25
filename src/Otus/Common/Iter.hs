module Otus.Common.Iter (
  Sized (..),
  AppendL (..),
  AppendR (..),
  Extend (..),
  SeqIndex (..),
  IndexableSeq (..),
) where

import qualified Data.List as L
import qualified Data.Sequence as Seq

-- Sized
class Sized a where
  size :: a -> Int

instance Sized (Seq.Seq a) where
  size = length

instance Sized [a] where
  size = length

-- Append & Extend
class AppendR l a where
  (|>) :: l -> a -> l

class AppendL l a where
  (<|) :: a -> l -> l

class Extend l r where
  (><) :: l -> r -> l

instance AppendR (Seq.Seq a) a where
  (|>) = (Seq.|>)

instance AppendL (Seq.Seq a) a where
  (<|) = (Seq.<|)

instance Extend (Seq.Seq a) (Seq.Seq a) where
  (><) = (Seq.><)

instance AppendR [a] a where
  l |> a = l ++ [a]

instance AppendL [a] a where
  (<|) = (:)

instance Extend [a] [a] where
  (><) = (++)

-- Index
class SeqIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  intoLeftIndex :: (Sized a) => a -> id -> Int
  intoRightIndex :: (Sized a) => a -> id -> Int

class (Sized l) => IndexableSeq l where
  type Item l
  (@?) :: (SeqIndex id) => l -> id -> Maybe (Item l)

instance IndexableSeq (Seq.Seq a) where
  type Item (Seq.Seq a) = a
  s @? idx = s Seq.!? intoLeftIndex s idx

instance IndexableSeq [a] where
  type Item [a] = a
  s @? idx = s L.!? intoLeftIndex s idx
