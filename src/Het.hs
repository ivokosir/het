module Het where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

data Signal a = Signal ((a -> STM ()) -> IO ())

instance Functor Signal where  
  fmap f (Signal run) = Signal $ \ write -> run $ write . f

instance Applicative Signal where
  pure a = Signal $ \ write -> atomically $ write a
  (Signal run1) <*> (Signal run2) = Signal $ \ write -> do
    v1 <- atomically newEmptyTMVar
    v2 <- atomically newEmptyTMVar
    forkIO . run1 $ forcePut v1
    forkIO . run2 $ forcePut v2
    forever . atomically $ do
      (x1, b1) <- readTMVar v1
      (x2, b2) <- readTMVar v2
      when b1 $ void $ swapTMVar v1 (x1, False)
      when b2 $ void $ swapTMVar v2 (x2, False)
      case (b1, b2) of
        (False, False) -> return ()
        _ -> write $ x1 x2
   where
    forcePut v x = do
      tryTakeTMVar v
      putTMVar v (x, True)

{- |Runs signal in new thread and returns 'System.IO.IO' function to read output.
Example of usage:

> read <- start someSignal
> print =<< read
> print =<< read
-}
start :: Signal a -> IO (IO a)
start (Signal run) = do
  v <- atomically newEmptyTMVar
  forkIO . run $ putTMVar v
  return . atomically $ takeTMVar v

helloSignal :: Signal String
helloSignal = Signal $ \ write -> forever $ do
  atomically (write "hello")
  threadDelay 250000

randomCycle :: Signal Int
randomCycle = Signal $ \ write -> forever $ do
  x <- randomRIO (0, 10)
  atomically (write x)
  threadDelay 1000000

