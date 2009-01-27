{-# OPTIONS_GHC -Wall -O2
  #-}

module List where
    isSorted :: (Ord a) => [a] -> Bool
    isSorted xs = and $ zipWith (<=) xs (tail xs)
