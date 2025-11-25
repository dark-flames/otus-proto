module Otus.Common.Result (
  Result (..),
  ResultT (..),
) where

import Control.Monad.Error.Class

data Result e a
  = Success a
  | Failure e
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative (Result e) where
  pure = Success
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
  (Success f) <*> (Success a) = Success (f a)

instance Monad (Result e) where
  (Failure e) >>= _ = Failure e
  (Success a) >>= f = f a

instance MonadError e (Result e) where
  throwError = Failure
  catchError (Failure e) handler = handler e
  catchError (Success a) _ = Success a

newtype ResultT e m a = ResultT
  { runResultT :: m (Result e a)
  }

instance (Functor m) => Functor (ResultT e m) where
  fmap f (ResultT a) = ResultT $ fmap (fmap f) a

instance (Applicative m) => Applicative (ResultT e m) where
  pure a = ResultT $ pure $ Success a
  tf <*> ta =
    ResultT $
      let rf = runResultT tf
      in fmap (<*>) rf <*> runResultT ta

instance (Monad m) => Monad (ResultT e m) where
  ta >>= f = ResultT $ do
    ra <- runResultT ta
    case ra of
      Failure e -> return $ Failure e
      Success y -> runResultT (f y)

instance (Monad m) => MonadError e (ResultT e m) where
  throwError = ResultT . pure . Failure
  catchError ra h =
    ResultT $
      runResultT ra >>= \case
        Failure e -> runResultT $ h e
        Success r -> pure $ Success r
