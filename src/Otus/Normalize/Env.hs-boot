module Otus.Normalize.Env (
  EnvItem,
  Environment (..),
) where

import qualified Data.Sequence as Seq

data EnvItem

instance Show EnvItem

instance Eq EnvItem

newtype Environment = Env (Seq.Seq EnvItem)

instance Show Environment

instance Eq Environment
