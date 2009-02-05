{-# OPTIONS -Wall -O2
  #-}

module Graphics.UI.LUI.Image
    (-- The compositional interface:
     Image
    ,text
    ,textSize
    ,rect
    ,move
    ,clip
    -- The implementational interface
    ,render
    ) where

import qualified Graphics.UI.HaskGame as HaskGame
import qualified Graphics.UI.HaskGame.Font as Font
import qualified Graphics.UI.HaskGame.Rect as Rect
import Graphics.UI.HaskGame(Surface)
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Graphics.UI.HaskGame.Color(Color)
import Graphics.UI.HaskGame.Font(Font)
import Graphics.UI.HaskGame.Rect(Rect(..), unionRects, intersectRects)
import Data.Monoid(Monoid(..))

data BBox = BBoxInf | BBox Rect

bboxSize :: Vector2 Int -> BBox
bboxSize size = BBox $ Rect.makeRect (Vector2 0 0) size

onBBoxRect :: (Rect -> Rect) -> BBox -> BBox
onBBoxRect _ BBoxInf = BBoxInf
onBBoxRect f (BBox r) = BBox $ f r

unionBBox, intersectBBox :: BBox -> BBox -> BBox
BBoxInf `unionBBox` _       = BBoxInf
_       `unionBBox` BBoxInf = BBoxInf
BBox r1 `unionBBox` BBox r2 = BBox $ r1 `unionRects` r2

BBoxInf   `intersectBBox` b2        = b2
b1        `intersectBBox` BBoxInf   = b1
(BBox r1) `intersectBBox` (BBox r2) = BBox $ r1 `intersectRects` r2

-- Image semantically represents an infinite map from pixel index to
-- color.
data Image = Image
    {
      imageBBox :: BBox
    , render :: Surface -> Vector2 Int -> IO ()
    }

instance Monoid Image where
    mempty = Image BBoxInf $ const . const . return $ ()
    Image xbbox xdraw `mappend` Image ybbox ydraw = Image bbox draw
        where
          bbox = xbbox `unionBBox` ybbox
          draw surface pos = do
            xdraw surface pos
            ydraw surface pos

-- Re-export this to go along with text below
textSize :: Font -> String -> Vector2 Int
textSize = Font.textSize

text :: Color -> Font -> String -> Image
text color font str = Image (bboxSize size) draw
    where
      size = textSize font str
      draw surface pos = do
        textSurface <- Font.renderText font str color
        HaskGame.blit surface pos textSurface

rect :: Color -> Vector2 Int -> Image
rect color size = Image (bboxSize size) draw
    where
      draw surface pos = do
        HaskGame.fillRect surface (Rect.makeRect pos size) color

move :: Vector2 Int -> Image -> Image
move delta image = Image bbox draw
    where
      bbox = onBBoxRect (Rect.rectPos (+delta)) $ imageBBox image
      draw surface pos = do
        render image surface (pos + delta)

clip :: Rect -> Image -> Image
clip clipRect (Image bbox draw) =
    Image (BBox clipRect `intersectBBox` bbox) draw
