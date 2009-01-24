{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
    -XFunctionalDependencies
  #-}

module Widget where

import qualified MySDLKey
import qualified Data.Map as Map
import Draw(Draw)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

class Widget w s | w -> s where
    getKeymap :: w -> s -> Map.Map
                             (KeyStatus, MySDLKey.KeyGroup)
                             (String, MySDLKey.Key -> s)
    draw :: w -> s -> Draw ()
