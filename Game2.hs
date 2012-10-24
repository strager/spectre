{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game2
  ( main
  ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Data.Maybe
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.IntMap as IntMap

-- Abstract.
newtype Update m a = U { unU :: ContT () (StateT UpdateState m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans Update where
  lift = U . lift . lift

type SourceX m a = UVar a -> Update m ()

-- Abstract.
data UpdateState = UpdateState
  { vars :: IntMap.IntMap (forall a. a)
  , result :: forall a. Maybe a
  }

-- | Suspends the current computation until the variable is
-- updated.
suspend
  :: (Monad m)
  => UVar a
  -> Update m ()
suspend = error "suspend: TODO"

-- | Executes in parallel with a new 'UVar' for
-- communication.
parallel
  :: (Monad m)
  => SourceX m a
  -> a
  -> Update m (UVar a)
parallel = error "parallel: TODO"

-- Abstract.
newtype UVar a = UVar Int

curID :: IORef Int
curID = unsafePerformIO $ newIORef 0
{-# NOINLINE curID #-}

nextID :: IO Int
nextID = atomicModifyIORef curID
  $ \ u -> (succ u, u)

newUVar :: (Monad m) => a -> Update m (UVar a)
newUVar x = do
  varID <- return $! unsafePerformIO nextID
  let var = UVar varID
  writeUVar var x
  return var

readUVar :: (Monad m) => UVar a -> Update m a
readUVar (UVar varID) = do
  s <- U $ lift get
  case IntMap.lookup varID (vars s) of
    Just x -> do
      U $ lift $ put s { vars = IntMap.insert varID x (vars s) }
      return $ unsafeCoerce x
    Nothing -> error "readUVar: UVar not associated with this update state"

awaitUVar :: (Monad m) => UVar a -> Update m a
awaitUVar (UVar varID) = error "awaitUVar: TODO"

writeUVar :: (Monad m) => UVar a -> a -> Update m ()
writeUVar (UVar varID) x
  = U $ lift $ modify $ \ s -> s
    { vars = IntMap.insert varID (unsafeCoerce x) (vars s) }

modifyUVar :: (Monad m) => UVar a -> (a -> a) -> Update m ()
modifyUVar var f = do
  x <- readUVar var
  writeUVar var (f x)

startUpdateT
  :: (Monad m)
  => Update m a
  -> m (a, UpdateState)
startUpdateT m = runUpdateT initState m
  where
    initState = UpdateState
      { vars = IntMap.empty
      , result = Nothing
      }

runUpdateT
  :: (Monad m)
  => UpdateState
  -> Update m a
  -> m (a, UpdateState)
runUpdateT state m = do
  state' <- flip execStateT state
    $ runContT (unU m)
    $ \ x -> modify (\ s -> s { result = Just $ unsafeCoerce x })

  return (unsafeCoerce . fromJust $ result state', state')

execUpdateT
  :: (Monad m)
  => UpdateState
  -> Update m ()
  -> m UpdateState
execUpdateT s m = liftM snd $ runUpdateT s m

data World = World (UVar String)

game :: World -> Update IO ()
game world = do
  scoreVar <- parallel (playerScore world) 0
  forever $ do
    score <- awaitUVar scoreVar
    lift $ print score

playerScore :: World -> SourceX IO Int
playerScore world scoreVar = forever $ do
  line <- inputLine world
  modifyUVar scoreVar (+ read line)

inputLine :: (Monad m) => World -> Update m String
inputLine (World inputLineVar) = awaitUVar inputLineVar

main :: IO ()
main = do
  (world, state) <- startUpdateT $ do
    inputVar <- newUVar "hello"
    let world = World inputVar
    game world
    return world

  putStrLn "Enter integers:"
  loop world state

  where
    loop world@(World inputVar) state = do
      line <- getLine
      state' <- execUpdateT state $ writeUVar inputVar line
      loop world state'
