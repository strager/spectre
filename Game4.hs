{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game4 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent hiding (yield)
import Control.Concurrent.Chan
import Control.Monad.Suspend
import Control.Monad.Vars
import Data.Maybe

newtype UpdateT source event m a
  = UpdateT (SuspendT event m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

awaitEvent :: (Monad m) => UpdateT s e m e
awaitEvent = UpdateT suspend

-- Stolen from Network.CGI.Protocol in package 'cgi'.
-- BSD license, (c) Bjorn Bringert 2006.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

data Void

newtype Source a = Source (Var a)

-- | Yields a value to the parent process.
yield :: (Monad m) => s -> UpdateT s e m ()
yield = error "yield"

-- | Blocks until data is received downstream.
block :: (Monad m) => UpdateT s e m a -> UpdateT s e m a
block = error "block"

-- | Asks for the current value of a source.
query :: (Monad m) => Source a -> UpdateT s e m a
query = error "query"

-- | Compute in parallel.  Interleave patterns of operations
-- in the inner monad is undefined.
parallel
  :: (Monad m)
  => s                -- ^ Initial value.
  -> UpdateT s e m a  -- ^ Parallel computation.
  -> UpdateT s' e m (Source s)
parallel z m = error "parallel"

readCurrentNumber :: (Monad m) => UpdateT Int String m Void
readCurrentNumber = forever $ do
  line <- awaitEvent
  case maybeRead line of
    Just number -> yield number
    Nothing -> return ()

gameLoop :: UpdateT () String IO ()
gameLoop = do
  curNumber <- parallel 0 readCurrentNumber
  forever . block $ do
    num <- query curNumber
    liftIO $ putStrLn $ "You typed the number: " ++ show num

streamForever
  :: (Monad m)
  => m e
  -> UpdateT s e m a
  -> m a
streamForever source (UpdateT m)
  = streamForeverSuspendT source m

main :: IO ()
main = do
  lines <- newChan :: IO (Chan String)
  void . forkIO . forever
    $ getLine >>= writeChan lines

  streamForever (readChan lines) gameLoop
