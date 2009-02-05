{-# OPTIONS -Wall -O2
  #-}

module Graphics.UI.LUI.Image
    (-- The compositional interface:
     Image
    ,text
    ,textSize
    ,rect
    ,move
    ,crop
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
import Graphics.UI.HaskGame.Rect(Rect(..), intersectRects)
import Data.Monoid(Monoid(..))

data BBox = BBoxInf | BBox Rect

-- bboxFromSize :: Vector2 Int -> BBox
-- bboxFromSize size = BBox $ Rect.makeRect (Vector2 0 0) size

-- onBBoxRect :: (Rect -> Rect) -> BBox -> BBox
-- onBBoxRect _ BBoxInf = BBoxInf
-- onBBoxRect f (BBox r) = BBox $ f r

bboxRect :: BBox -> Maybe Rect
bboxRect BBoxInf = Nothing
bboxRect (BBox r) = Just r

-- unionBBox :: BBox -> BBox -> BBox
-- BBoxInf `unionBBox` _       = BBoxInf
-- _       `unionBBox` BBoxInf = BBoxInf
-- BBox r1 `unionBBox` BBox r2 = BBox $ r1 `unionRects` r2

intersectBBox :: BBox -> BBox -> BBox
BBoxInf   `intersectBBox` b2        = b2
b1        `intersectBBox` BBoxInf   = b1
(BBox r1) `intersectBBox` (BBox r2) = BBox $ r1 `intersectRects` r2

-- Image semantically represents an infinite map from pixel index to
-- color.
data Image = Image { imageDraw :: Surface -> Vector2 Int -> BBox -> IO () }

render :: Image -> Surface -> Vector2 Int -> IO ()
render image surface pos = imageDraw image surface pos BBoxInf

instance Monoid Image where
    mempty = Image (const . const . const . return $ ())
    Image xdraw `mappend` Image ydraw = Image draw
        where
          draw surface pos bbox = xdraw surface pos bbox >>
                                  ydraw surface pos bbox

-- Re-export this to go along with text below
textSize :: Font -> String -> Vector2 Int
textSize = Font.textSize

text :: Color -> Font -> String -> Image
text color font str = Image draw
    where
      draw surface pos bbox = do
        textSurface <- Font.renderText font str color
        let blit = maybe HaskGame.blit HaskGame.blitPart $ bboxRect bbox
        blit surface pos textSurface

rect :: Color -> Vector2 Int -> Image
rect color size = Image draw
    where
      draw surface pos bbox =
        let origRect = Rect.makeRect pos size
            finalRect = maybe origRect (origRect `intersectRects`) $ bboxRect bbox
        in HaskGame.fillRect surface finalRect color

move :: Vector2 Int -> Image -> Image
move delta (Image xdraw) = Image draw
    where
      draw surface pos = xdraw surface (pos + delta)

crop :: Rect -> Image -> Image
crop cropRect (Image xdraw) = Image draw
    where
      draw surface pos bbox = xdraw surface pos (BBox cropRect `intersectBBox` bbox)
