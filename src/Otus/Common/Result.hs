module Otus.Common.Result(
  Result(..)
) where

import           Control.Monad.Error.Class

data Result e a
  = Success a
  | Failure e
  deriving (Show, Eq)

instance Functor (Result e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative (Result e) where
  pure = Success
  (Failure e) <*> _           = Failure e
  _ <*> (Failure e)           = Failure e
  (Success f) <*> (Success a) = Success (f a)

instance Monad (Result e) where
  return = pure
  (Failure e) >>= _ = Failure e
  (Success a) >>= f = f a

instance MonadError e (Result e) where
  throwError = Failure
  catchError (Failure e) handler = handler e
  catchError (Success a) _       = Success a
