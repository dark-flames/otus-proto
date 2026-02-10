module Otus.Common.Seq (
  Indexable (..),
  SeqIndex (..),
  Sized (..),
  Seq (..),
  (|>),
  (<|),
  (><),
  foldlM,
) where

import Data.Sequence (Seq (..), (<|), (><), (|>))

import qualified Data.Sequence as Seq

class Sized l where
  size :: l -> Int

instance Sized (Seq a) where
  size = Seq.length

-- Index
class SeqIndex id where
  shift :: Int -> id -> id
  sub :: id -> id -> Int
  intoLeftIndex :: (Sized l) => l -> id -> Int
  intoRightIndex :: (Sized l) => l -> id -> Int

instance SeqIndex Int where
  shift = (+)
  sub = (-)
  intoLeftIndex = const id
  intoRightIndex l i = size l - i - 1

class (Sized l) => Indexable l where
  type Item l

  (@?) :: (SeqIndex id) => l -> id -> Maybe (Item l)

instance Indexable (Seq a) where
  type Item (Seq a) = a
  l @? idx = l Seq.!? intoLeftIndex l idx

foldlM :: (Monad m) => (a -> b -> m a) -> a -> Seq b -> m a
foldlM f i = foldl go (pure i)
  where
    go prev e = prev >>= (`f` e)
