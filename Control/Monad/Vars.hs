{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Vars
  ( VarsT
  , runVarsT

  , Var
  , newVar
  , readVar
  , writeVar
  , modifyVar
  , modifyVar_
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.IntMap as IntMap

type VarState = IntMap.IntMap (forall a. a)

newtype VarsT m a = VarsT
  { runVarsT :: StateT VarState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

newtype Var a = Var Int

curID :: IORef Int
curID = unsafePerformIO $ newIORef 0
{-# NOINLINE curID #-}

nextID :: IO Int
nextID = atomicModifyIORef curID
  $ \ u -> (succ u, u)

newVar :: (Monad m) => a -> VarsT m (Var a)
newVar x = do
  varID <- return $! unsafePerformIO nextID
  let var = Var varID
  writeVar var x
  return var

readVar :: (Monad m) => Var a -> VarsT m a
readVar (Var varID) = do
  vars <- VarsT get
  case IntMap.lookup varID vars of
    Just x -> do
      VarsT . put $ IntMap.insert varID x vars
      return $ unsafeCoerce x
    Nothing -> error "readVar: Var not associated with this update state"

writeVar :: (Monad m) => Var a -> a -> VarsT m ()
writeVar (Var varID) x
  = VarsT . modify $ IntMap.insert varID (unsafeCoerce x)

modifyVar
  :: (Monad m)
  => Var a
  -> (a -> VarsT m (a, b))
  -> VarsT m b
modifyVar var f = do
  x <- readVar var
  (x', ret) <- f x
  writeVar var x'
  return ret

modifyVar_
  :: (Monad m)
  => Var a
  -> (a -> VarsT m a)
  -> VarsT m ()
modifyVar_ var f
  = readVar var >>= f >>= writeVar var
