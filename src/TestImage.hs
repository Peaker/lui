{-# OPTIONS_GHC -Wall -O2 #-}
import qualified Graphics.UI.LUI.Image as Image
import qualified Graphics.UI.HaskGame as HaskGame
import qualified Graphics.UI.HaskGame.Font as Font
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.HaskGame.Color(Color(..))
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Data.Monoid(Monoid(..))
import Control.Monad(forM_)

handleEvents :: [HaskGame.Event] -> Bool
handleEvents = (SDL.Quit `elem`)

main :: IO ()
main = HaskGame.withInit $ do
  display <- HaskGame.setVideoMode 800 600 16
  font <- Font.defaultFont 100
  forM_ [0..] $ \i -> do
     events <- HaskGame.getEvents
     if handleEvents events
       then
           fail "Blah"
       else
           return ()
     HaskGame.fillSurface display (Color 0 0 0)
     let borderI = 30
         borderIHalf = borderI `div` 2
         border = Vector2 borderI borderI
         halfBorder = Vector2 borderIHalf borderIHalf
         iMod = Vector2 (i `mod` 130) (i `mod` 130)
         text = "X"
         textImg = Image.move halfBorder .
                   Image.text (Color 0 255 0) font $
                   text
         textSize = Image.textSize font text
         rectImg = Image.rect (Color 255 100 100) $
                   border + textSize
         img = Image.crop iMod .
               Image.move halfBorder $
               rectImg `mappend` textImg
     Image.render img display $ Vector2 0 0
     SDL.flip display
     SDL.delay 100
