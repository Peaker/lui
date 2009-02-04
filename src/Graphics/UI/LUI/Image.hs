{-# OPTIONS -Wall -O2
  #-}

module Graphics.UI.LUI.Image
    (-- The compositional interface:
     Image
    ,text
    ,textSize
    ,rect
    ,move
    -- The implementational interface
    ,Pos,render
    ) where

import qualified Graphics.UI.HaskGame as HaskGame
import qualified Graphics.UI.HaskGame.Font as Font
import qualified Graphics.UI.HaskGame.Rect as Rect
import Graphics.UI.HaskGame(Surface)
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Graphics.UI.HaskGame.Color(Color)
import Graphics.UI.HaskGame.Font(Font)
import Graphics.UI.HaskGame(Size)
import Data.Monoid(Monoid(..))

type Pos = Vector2 Int

-- Image semantically represents an infinite map from pixel index to
-- color.
newtype Image = Image
    {
      render :: Surface -> Pos -> IO ()
    }

instance Monoid Image where
    mempty = Image $ const . const . return $ ()
    Image xdraw `mappend` Image ydraw = Image draw
        where
          draw surface pos = do
            xdraw surface pos
            ydraw surface pos

-- Re-export this to match text below
textSize :: Font -> String -> Size
textSize = Font.textSize

text :: Color -> Font -> String -> Image
text color font str = Image draw
    where
      draw surface pos = do
        textSurface <- Font.renderText font str color
        HaskGame.blit surface pos textSurface

rect :: Color -> Size -> Image
rect color size = Image draw
    where
      draw surface pos = do
        HaskGame.fillRect surface (Rect.makeRect pos size) color

move :: Pos -> Image -> Image
move delta image = Image draw
    where
      draw surface pos = do
        render image surface (pos + delta)
