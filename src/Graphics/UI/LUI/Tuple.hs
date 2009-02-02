{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Tuple where
    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)
