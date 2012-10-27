{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game4 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Suspend
-- import Control.Monad.Vars

newtype UpdateT source event m a
  = UpdateT (SuspendT event m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

awaitEvent :: (Monad m) => UpdateT s e m e
awaitEvent = UpdateT suspend

gameLoop :: UpdateT () String IO ()
gameLoop = forever $ do
  line <- awaitEvent
  liftIO $ putStrLn $ "Received event: " ++ line

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
