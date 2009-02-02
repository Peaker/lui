{-# OPTIONS_GHC -Wall -O2
  #-}

-- TODO: Put this in a more generic library

module Graphics.UI.LUI.List(isSorted, nth) where
    isSorted :: (Ord a) => [a] -> Bool
    isSorted xs = and $ zipWith (<=) xs (tail xs)

    -- Semantic editor combinator, like "first" or "second", but for
    -- an index in a list
    nth :: Int -> (a -> a) -> [a] -> [a]
    nth _ _    [] = error "nth index out of range"
    nth 0 func (x:xs) = func x:xs
    nth n func (x:xs) = x:nth (n-1) func xs
