{-# OPTIONS_GHC -Wall -O2
  #-}

-- TODO: Put this in a more generic library

module Graphics.UI.LUI.Tuple(swap) where
    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)
