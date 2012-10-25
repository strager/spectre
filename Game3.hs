module Game3 where

import Control.Monad
import Control.Monad.Trans.Class

newtype SuspendT k m a = SuspendT
  { runSuspendT :: m (Either (k -> SuspendT k m a) a) }

instance (Monad m) => Monad (SuspendT k m) where
  return x = SuspendT $ return (Right x)
  m >>= f = SuspendT $ do
    mX <- runSuspendT m
    case mX of
      Left suspension -> return $ Left $ \ k -> suspension k >>= f
      Right x -> runSuspendT (f x)

instance MonadTrans (SuspendT k) where
  lift m = SuspendT $ liftM Right m

suspend :: (Monad m) => SuspendT a m a
suspend = SuspendT $ return $ Left return

mytest :: SuspendT Int IO ()
mytest = do
  lift $ putStrLn "mytest"

  x <- suspend
  lift $ putStrLn $ "got: " ++ show x

main :: IO ()
main = do
  t <- runSuspendT mytest
  case t of
    Left f -> do
      t' <- runSuspendT $ f 42
      case t' of
        Left _ -> fail "Got left"
        Right x -> putStrLn $ "got: " ++ show x

    Right x -> do
      fail $ "Got right: " ++ show x
