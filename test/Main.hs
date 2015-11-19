module Main where

import Control.Applicative
import Control.Monad

import Het

main = do
  read <- start $
    keepWhen ((<3) <$> foldp (\ _ i -> i + 1) 0 timeoutSignal) helloSignal
  forever $ print =<< read

