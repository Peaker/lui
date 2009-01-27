{-# OPTIONS_GHC -Wall -O2
  #-}

module Trace where

import Debug.Trace(trace)

traceId :: Show a => String -> a -> a
traceId prefix x = trace (prefix ++ show x) x
