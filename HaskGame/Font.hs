{-# OPTIONS -Wall -O2 #-}

module HaskGame.Font(Font, renderText,textSize,defaultFont,withInit)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified HaskGame.Utils as Utils
import HaskGame.Color(Color(..))
import HaskGame.Vector2(Vector2(..))

type Font = TTF.Font

renderText :: Font -> String -> Color -> IO SDL.Surface
renderText font text (Color r g b) = if null text
                             then
                                 SDL.createRGBSurface [] 0 0 0 0 0 0 0
                             else
                                 TTF.renderTextBlended font text (SDL.Color r g b)

textSize :: Font -> String -> IO (Vector2 Int)
textSize font text = do
  (w, h) <- TTF.textSize font text
  return $ Vector2 w h

defaultFont :: Int -> IO Font
defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

withInit :: IO () -> IO ()
withInit = Utils.bracket__ (Utils.ioBoolToError "TTF init failure" TTF.init) TTF.quit
