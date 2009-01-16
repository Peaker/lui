{-# OPTIONS_GHC -Wall -XDeriveDataTypeable #-}

module Widget where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym(SDLKey)
import qualified HierMap

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

class Widget w where
    getKeymap :: w -> IO (HierMap.HierMap (KeyStatus, SDLKey) (String, IO ()))
    draw :: w -> SDL.Rect -> SDL.Surface -> IO ()
