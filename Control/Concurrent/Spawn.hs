{-# LANGUAGE
    CPP #-}
module Control.Concurrent.Spawn
  ( -- * Spawn 
    spawn

    -- * Spawn with @'try'@
  , Result
  , spawnTry
  
    -- * Limiting concurrency
  , pool ) where

import Control.Concurrent
import Control.Exception

-- | Two ways a computation of type @'IO' a@ can end.
type Result a = Either SomeException a

-- | Spawn a concurrent computation.  Produces an action which
-- demands a @'Result'@.
spawnTry :: IO a -> IO (IO (Result a))

-- We block asynchronous exceptions around 'forkIO', then restore
-- the parent thread's exception mask state inside 'try'. This
-- prevents an exception from interrupting 'putMVar', which would
-- deadlock the parent.
--
-- The API for doing this changed in base-4.3 with GHC 7.0.

#if MIN_VERSION_base(4,3,0)
spawnTry m = do
  v <- newEmptyMVar
  _ <- mask $ \restore -> forkIO (try (restore m) >>= putMVar v)
  return (readMVar v)

#else
spawnTry m = do
  v <- newEmptyMVar
  b <- blocked
  _ <- block $ forkIO (try (if b then m else unblock m) >>= putMVar v)
  return (readMVar v)

#endif

-- | Spawn a concurrent computation.  Produces an action which
-- demands the result.  Any exception from the original computation
-- is re-thrown when and where the result is demanded.
spawn :: IO a -> IO (IO a)
spawn m = do
  r <- spawnTry m
  return (r >>= either throwIO return)

-- | Given /n/, produces a function to wrap @'IO'@ actions.
-- No more than /n/ wrapped actions will be in progress at
-- one time.
pool :: Int -> IO (IO a -> IO a)
pool n = do
  s <- newQSem n
  return $ bracket_ (waitQSem s) (signalQSem s)
