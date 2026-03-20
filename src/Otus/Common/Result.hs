{-# LANGUAGE InstanceSigs #-}

module Otus.Common.Result (
  Result (..),
  ResultT (..),
) where

import Control.Monad.Error.Class
import Control.Monad.Trans.Class

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

instance Foldable (Result e) where
  foldMap f = \case
    (Failure _) -> mempty
    (Success a) -> f a

  foldr f z = \case
    (Failure _) -> z
    (Success a) -> f a z

  length = \case
    (Failure _) -> 0
    (Success _) -> 1

instance Traversable (Result e) where
  traverse :: (Applicative f) => (a -> f b) -> Result e a -> f (Result e b)
  traverse f = \case
    (Failure e) -> pure $ Failure e
    (Success a) -> Success <$> f a

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

instance MonadTrans (ResultT e) where
  lift :: (Monad m) => m a -> ResultT e m a
  lift = ResultT . fmap Success

instance (Foldable m) => Foldable (ResultT e m) where
  foldMap :: (Monoid n) => (a -> n) -> ResultT e m a -> n
  foldMap f r = foldMap (foldMap f) $ runResultT r

  foldr f z r = foldr f' z $ runResultT r
    where
      f' r' b = foldr f b r'

instance (Traversable m) => Traversable (ResultT e m) where
  traverse
    :: (Applicative f)
    => (a -> f b) -> ResultT e m a -> f (ResultT e m b)
  traverse f r = ResultT <$> traverse (traverse f) (runResultT r)
