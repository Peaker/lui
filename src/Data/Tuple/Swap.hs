{-# OPTIONS_GHC -Wall -O2 #-}

-- TODO: Put this in a more generic library

module Data.Tuple.Swap(swap) where
    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)
