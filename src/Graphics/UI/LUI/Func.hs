{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Func where

import Control.Arrow(Arrow, (<<<), (>>>))

result :: Arrow arr => arr b c -> arr a b -> arr a c
result = (<<<)

argument :: Arrow arr => arr a b -> arr b c -> arr a c
argument = (>>>)

infixr 2 ~>
(~>) :: (Arrow arr) => arr a b -> arr c d -> arr b c -> arr a d
(arg ~> res) func = arg >>> func >>> res
