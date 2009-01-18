{-# OPTIONS_GHC -Wall -O2 #-}

module Widget where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import Graphics.UI.SDL.Keysym(SDLKey)
import qualified HierMap

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

type Action = (String, IO ())

class Widget w where
    getKeymap :: w -> IO (HierMap.HierMap (KeyStatus, MySDLKey.Mods, SDLKey) Action)
    draw :: w -> MySDL.Vector2 Int -> SDL.Surface -> IO ()
