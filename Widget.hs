{-# OPTIONS_GHC -Wall #-}

module Widget where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import Graphics.UI.SDL.Keysym(SDLKey)
import qualified HierMap

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

class Widget w where
    getKeymap :: w -> IO (HierMap.HierMap (KeyStatus, MySDLKey.Mods, SDLKey) (String, IO ()))
    draw :: w -> MySDL.Vector2 Int -> SDL.Surface -> IO ()
