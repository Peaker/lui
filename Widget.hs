{-# OPTIONS_GHC -Wall -O2 #-}

module Widget where

import qualified Graphics.UI.SDL as SDL
import qualified MySDLKey
import qualified HierMap
import Graphics.UI.SDL.Keysym(SDLKey)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

type Action = (String, IO ())

class Widget w where
    getKeymap :: w -> IO (HierMap.HierMap (KeyStatus, MySDLKey.Mods, SDLKey) Action)
    draw :: w -> IO SDL.Surface
