{-# OPTIONS -Wall -O2 #-}

module HaskGame.Font(renderText,textSize,defaultFont,withInit)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified HaskGame.Utils as Utils
import qualified HaskGame.Color as Color
import HaskGame.Vector2(Vector2(..))

renderText :: TTF.Font -> String -> Color.Color -> IO SDL.Surface
renderText font text (Color.Color r g b) = if null text
                             then
                                 SDL.createRGBSurface [] 0 0 0 0 0 0 0
                             else
                                 TTF.renderTextBlended font text (SDL.Color r g b)

textSize :: TTF.Font -> String -> IO (Vector2 Int)
textSize font text = do
  (w, h) <- TTF.textSize font text
  return $ Vector2 w h

defaultFont :: Int -> IO TTF.Font
defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

withInit :: IO () -> IO ()
withInit = Utils.bracket__ (Utils.ioBoolToError "TTF init failure" TTF.init) TTF.quit
