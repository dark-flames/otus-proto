module Otus.Normalize.Env (
  Environment (..),
) where

import qualified Data.Sequence as Seq

import {-# SOURCE #-} Otus.Normalize.Value

newtype Environment = Env (Seq.Seq Value)

instance Show Environment

instance Eq Environment
