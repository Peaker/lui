{-# OPTIONS_GHC -Wall -O2
  #-}

module Tuple where
    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)
