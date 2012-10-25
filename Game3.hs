module Game3 where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Suspend

mytest :: SuspendT Int IO ()
mytest = do
  lift $ putStrLn "mytest"

  x <- suspend
  lift $ putStrLn $ "got: " ++ show x

main :: IO ()
main = do
  print =<< feedSuspendT [42] mytest
  putStrLn ""

  print =<< feedSuspendT [] mytest
  putStrLn ""

  print =<< feedSuspendT [10, 50, 2923] mytest
  putStrLn ""

  var <- newMVar 9001
  print =<< streamSuspendT (tryTakeMVar var) (mytest >> mytest)
  putStrLn ""
