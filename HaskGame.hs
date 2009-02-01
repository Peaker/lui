{-# OPTIONS -Wall -O2 #-}

module HaskGame(Surface,Event
               ,createRGBSurface,blit,fillRect,fillSurface
               ,withInit,getEvents,surfaceSize,setVideoMode
               )
where

import qualified Graphics.UI.SDL as SDL
import qualified HaskGame.Rect as Rect
import qualified HaskGame.Font as Font
import qualified HaskGame.Utils as Utils
import HaskGame.Color(Color(..))
import HaskGame.Vector2(Vector2(..))
import Control.Monad(liftM)

-- Commented out for 6.8's lack of the new Exception
-- import Control.Exception(throwIO)

type Surface = SDL.Surface
type Event = SDL.Event

whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM cond element = do
  value <- element
  if cond value
    then liftM (value:) (whileM cond element)
    else return []

createRGBSurface :: Vector2 Int -> IO Surface
createRGBSurface (Vector2 w h) = do
  surface <- SDL.createRGBSurface [SDL.SWSurface]
     w h 32 0xFF 0xFF00 0xFF0000 0x00000000
  SDL.displayFormatAlpha surface

blit :: Surface -> Vector2 Int -> Surface -> IO ()
blit dest pos src = do
  SDL.blitSurface src Nothing dest (Just . Rect.makePosRect $ pos)
  return ()

sdlFillRect :: Surface -> Maybe Rect.Rect -> Color -> IO ()
sdlFillRect surface mRect color = do
  fillerPixel <- pixel surface color
  SDL.fillRect surface mRect fillerPixel
  return ()

fillSurface :: Surface -> Color -> IO ()
fillSurface surface color = sdlFillRect surface Nothing color

fillRect :: Surface -> Rect.Rect -> Color -> IO ()
fillRect surface rect color = sdlFillRect surface (Just rect) color

initKeyRepeat :: IO ()
initKeyRepeat = Utils.ioBoolToError "enableKeyRepeat failed" $ SDL.enableKeyRepeat 150 10

withInit :: IO () -> IO ()
withInit = SDL.withInit [SDL.InitEverything] .
           Utils.bracket__ initKeyRepeat (return ()) .
           Font.withInit

pixel :: Surface -> Color -> IO SDL.Pixel
pixel surface (Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

getEvents :: IO [Event]
getEvents = whileM (/=SDL.NoEvent) SDL.pollEvent

surfaceSize :: Surface -> Vector2 Int
surfaceSize surface = Vector2 (SDL.surfaceGetWidth surface)
                              (SDL.surfaceGetHeight surface)

setVideoMode :: Int -> Int -> Int -> IO Surface
setVideoMode xres yres colorDepth = SDL.setVideoMode xres yres colorDepth [SDL.DoubleBuf]
