{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
    -XFunctionalDependencies
  #-}

module Widget where

import qualified MySDLKey
import qualified Data.Map as Map
import Graphics.UI.SDL.Keysym(SDLKey)
import Draw(Draw)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

class Widget w s | w -> s where
    getKeymap :: w -> s -> Map.Map
                             (KeyStatus, MySDLKey.Mods, SDLKey)
                             (String, s)
    draw :: w -> s -> Draw ()
