module Otus.Normalize.Env (
  Environment (..),
) where

import Otus.Ast
import {-# SOURCE #-} Otus.Normalize.Value

import qualified Data.List as List

newtype Environment = Env [Value]
  deriving (Eq, Show)

instance Contextual Environment where
  ctxLength (Env vals) = length vals

instance CtxLike Environment Value where
  (Env vals) !? i = (List.!?) vals i

  push (Env vals) val = Env (val : vals)

  pushL val (Env vals) = Env (vals ++ [val])

  pushL' newVals (Env vals) = Env (vals ++ newVals)
