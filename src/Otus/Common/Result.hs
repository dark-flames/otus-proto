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
  fmap :: (a -> b) -> Result e a -> Result e b
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative (Result e) where
  pure :: a -> Result e a
  pure = Success

  (<*>) :: Result e (a -> b) -> Result e a -> Result e b
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
  (Success f) <*> (Success a) = Success (f a)

instance Monad (Result e) where
  (>>=) :: Result e a -> (a -> Result e b) -> Result e b
  (Failure e) >>= _ = Failure e
  (Success a) >>= f = f a

instance MonadError e (Result e) where
  throwError :: e -> Result e a
  throwError = Failure

  catchError :: Result e a -> (e -> Result e a) -> Result e a
  catchError (Failure e) handler = handler e
  catchError (Success a) _ = Success a

instance Foldable (Result e) where
  foldMap :: (Monoid m) => (a -> m) -> Result e a -> m
  foldMap f = \case
    (Failure _) -> mempty
    (Success a) -> f a

  foldr :: (a -> b -> b) -> b -> Result e a -> b
  foldr f z = \case
    (Failure _) -> z
    (Success a) -> f a z

  length :: Result e a -> Int
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
  fmap :: (a -> b) -> ResultT e m a -> ResultT e m b
  fmap f (ResultT a) = ResultT $ fmap (fmap f) a

instance (Applicative m) => Applicative (ResultT e m) where
  pure :: a -> ResultT e m a
  pure a = ResultT $ pure $ Success a
  (<*>) :: ResultT e m (a -> b) -> ResultT e m a -> ResultT e m b
  tf <*> ta =
    ResultT $
      let rf = runResultT tf
      in fmap (<*>) rf <*> runResultT ta

instance (Monad m) => Monad (ResultT e m) where
  (>>=) :: ResultT e m a -> (a -> ResultT e m b) -> ResultT e m b
  ta >>= f = ResultT $ do
    ra <- runResultT ta
    case ra of
      Failure e -> return $ Failure e
      Success y -> runResultT (f y)

instance (Monad m) => MonadError e (ResultT e m) where
  throwError :: e -> ResultT e m a
  throwError = ResultT . pure . Failure

  catchError :: ResultT e m a -> (e -> ResultT e m a) -> ResultT e m a
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

  foldr :: (a -> b -> b) -> b -> ResultT e m a -> b
  foldr f z r = foldr f' z $ runResultT r
    where
      f' r' b = foldr f b r'

instance (Traversable m) => Traversable (ResultT e m) where
  traverse :: (Applicative f) => (a -> f b) -> ResultT e m a -> f (ResultT e m b)
  traverse f r = ResultT <$> traverse (traverse f) (runResultT r)
