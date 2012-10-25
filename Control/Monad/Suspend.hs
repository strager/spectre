module Control.Monad.Suspend
  ( Suspended(..)
  , SuspendT
  , runSuspendT
  , suspend

  , feedSuspendT
  , streamForeverSuspendT
  , streamSuspendT
  ) where

import Control.Monad
import Control.Monad.Trans.Class

-- | The result of 'SuspendT', or a continuation.
--
-- Isomorphic to Either (k -> SuspendT k m a) a.
data Suspended k m a
  = Result a
  | Suspended (k -> SuspendT k m a)

instance (Show a) => Show (Suspended k m a) where
  show (Result x) = "Result " ++ show x
  show (Suspended _) = "Suspended"

newtype SuspendT k m a = SuspendT
  { runSuspendT :: m (Suspended k m a) }

instance (Monad m) => Monad (SuspendT k m) where
  return x = SuspendT . return
    $ Result x

  m >>= f = SuspendT $ do
    mX <- runSuspendT m
    case mX of
      Suspended cont -> return . Suspended
        $ \ k -> cont k >>= f
      Result x -> runSuspendT (f x)

instance MonadTrans (SuspendT k) where
  lift m = SuspendT $ liftM Result m

suspend :: (Monad m) => SuspendT a m a
suspend = SuspendT . return
  $ Suspended return

-- | Feeds a list of values into a suspension.
feedSuspendT
  :: (Monad m)
  => [k]
  -> SuspendT k m a
  -> m (Suspended k m (a, [k]))
feedSuspendT ks m = runSuspendT m >>= \ r -> case r of
  Result x -> return $ Result (x, ks)
  Suspended cont -> case ks of
    [] -> return . Suspended
      $ \ k -> cont k >>= \ x -> return (x, [])
    (k:ks') -> feedSuspendT ks' (cont k)

-- | Feeds values from a monad into a suspension forever.
streamForeverSuspendT
  :: (Monad m)
  => m k
  -> SuspendT k m a
  -> m a
streamForeverSuspendT source m = runSuspendT m >>= \ r -> case r of
  Result x -> return x
  Suspended cont -> source >>= streamForeverSuspendT source . cont

-- | Feeds values from a monad into a suspension, with
-- cancellation.
streamSuspendT
  :: (Monad m)
  => m (Maybe k)
  -> SuspendT k m a
  -> m (Suspended k m a)
streamSuspendT source m = runSuspendT m >>= \ r -> case r of
  Result x -> return $ Result x
  Suspended cont -> source >>= maybe
    (return $ Suspended cont)
    (streamSuspendT source . cont)
