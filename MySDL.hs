{-# OPTIONS -Wall -O2 #-}

module MySDL where

import Data.Word(Word8)
import qualified MyMonad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified IO
import Vector2(Vector2(..), vector2first, vector2second)
import Control.Exception(throwIO)
import Control.Arrow(first, second)
import Control.Applicative(liftA2)

import qualified Graphics.UI.SDL.Utilities as Utils
allValues :: (Bounded a, Utils.Enum a v) => [a]
allValues = Utils.enumFromTo minBound maxBound

type ColorDepth = Int
createRGBSurface :: Vector2 Int -> IO SDL.Surface
createRGBSurface (Vector2 w h) = do
  surface <- SDL.createRGBSurface [SDL.SWSurface]
     w h 32 0xFF 0xFF00 0xFF0000 0x00000000
  SDL.displayFormatAlpha surface

blit :: SDL.Surface -> Vector2 Int -> SDL.Surface -> IO ()
blit dest pos src = do
  SDL.blitSurface src Nothing dest (Just . makePosRect $ pos)
  return ()

fillRect :: SDL.Surface -> SDL.Rect -> Color -> IO ()
fillRect surface rect color = do
  pixel <- MySDL.sdlPixel surface color
  SDL.fillRect surface (Just $ rect) pixel
  return ()

renderText :: TTF.Font -> String -> SDL.Color -> IO SDL.Surface
renderText font text color = if null text
                             then
                                 SDL.createRGBSurface [] 0 0 0 0 0 0 0
                             else
                                 TTF.renderTextBlended font text color

textSize :: TTF.Font -> String -> IO (Vector2 Int)
textSize font text = do
  (w, h) <- TTF.textSize font text
  return $ Vector2 w h

defaultFont :: Int -> IO TTF.Font
defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

doNothing :: IO ()
doNothing = return ()

ioBoolToError :: String -> IO Bool -> IO ()
ioBoolToError errStr act = do
  isSuccess <- act
  if isSuccess
    then return ()
    else throwIO . userError $ errStr

initKeyRepeat :: IO ()
initKeyRepeat = ioBoolToError "enableKeyRepeat failed" $ SDL.enableKeyRepeat 150 10

bracket__ :: IO () -> IO () -> IO () -> IO ()
bracket__ pre post code = IO.bracket_ pre (const post) code

withSDL :: IO () -> IO ()
withSDL = SDL.withInit [SDL.InitEverything] .
          bracket__ initKeyRepeat doNothing .
          bracket__ (ioBoolToError "TTF init failure" TTF.init) TTF.quit

type Color = (Word8, Word8, Word8)
sdlColor :: Color -> SDL.Color
sdlColor (r, g, b) = (SDL.Color r g b)
sdlPixel :: SDL.Surface -> Color -> IO SDL.Pixel
sdlPixel surface (r, g, b) = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

getEvents :: IO [SDL.Event]
getEvents = MyMonad.takeWhileM (return . (/=SDL.NoEvent)) SDL.pollEvent

surfaceSize :: SDL.Surface -> Vector2 Int
surfaceSize surface = Vector2 (SDL.surfaceGetWidth surface)
                              (SDL.surfaceGetHeight surface)


type Endo a = a -> a
type Two a = (a, a)

rectToVectors :: SDL.Rect -> Two (Vector2 Int)
rectToVectors (SDL.Rect x y w h) = (Vector2 x y, Vector2 w h)

vectorsToRect :: Two (Vector2 Int) -> SDL.Rect
vectorsToRect (Vector2 x y, Vector2 w h) = SDL.Rect x y w h

makeRect :: Vector2 Int -> Vector2 Int -> SDL.Rect
makeRect = curry vectorsToRect

-- Rect to Positional vectors
vectorsToPVectors, pVectorsToVectors :: Endo (Two (Vector2 Int))
vectorsToPVectors (p, s) = (p, p+s)
pVectorsToVectors (p1, p2) = (p1, p2-p1)

inRect :: Endo (Two (Vector2 Int)) -> Endo SDL.Rect
inRect f = vectorsToRect . f . rectToVectors

rectPos, rectSize :: Endo (Vector2 Int) -> Endo SDL.Rect
rectPos = inRect . first
rectSize = inRect . second

rectX, rectY, rectW, rectH :: Endo Int -> Endo SDL.Rect
rectX = rectPos . vector2first
rectY = rectPos . vector2second
rectW = rectSize . vector2first
rectH = rectSize . vector2second

makePosRect :: Vector2 Int -> SDL.Rect
makePosRect (Vector2 x y) = SDL.Rect x y 0 0

unionRects :: SDL.Rect -> SDL.Rect -> SDL.Rect
unionRects r1 r2 =
    let (tl1, br1) = vectorsToPVectors . rectToVectors $ r1
        (tl2, br2) = vectorsToPVectors . rectToVectors $ r2
    in vectorsToRect . pVectorsToVectors $
           ((liftA2 min tl1 tl2),
            (liftA2 max br1 br2))
