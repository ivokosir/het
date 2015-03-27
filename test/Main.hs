module Main where

import Control.Applicative
import Control.Monad

import Het

main = do
  read <- start $ (\ str int -> str ++ show int) <$> helloSignal <*> randomCycle
  forever $ print =<< read

