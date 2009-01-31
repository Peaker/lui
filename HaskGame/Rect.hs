module HaskGame.Rect(Rect
                    ,rectToVectors
                    ,vectorsToRect
                    ,makeRect
                    ,vectorsToPVectors, pVectorsToVectors
                    ,inRect
                    ,rectPos, rectSize
                    ,rectX, rectY, rectW, rectH
                    ,makePosRect
                    ,unionRects)
where

import qualified Graphics.UI.SDL as SDL
import HaskGame.Vector2(Vector2(..), vector2first, vector2second)
import Control.Arrow(first, second)
import Control.Applicative(liftA2)

type Rect = SDL.Rect
type Endo a = a -> a
type Two a = (a, a)

rectToVectors :: Rect -> Two (Vector2 Int)
rectToVectors (SDL.Rect x y w h) = (Vector2 x y, Vector2 w h)

vectorsToRect :: Two (Vector2 Int) -> Rect
vectorsToRect (Vector2 x y, Vector2 w h) = SDL.Rect x y w h

makeRect :: Vector2 Int -> Vector2 Int -> Rect
makeRect = curry vectorsToRect

-- Rect to Positional vectors
vectorsToPVectors, pVectorsToVectors :: Endo (Two (Vector2 Int))
vectorsToPVectors (p, s) = (p, p+s)
pVectorsToVectors (p1, p2) = (p1, p2-p1)

inRect :: Endo (Two (Vector2 Int)) -> Endo Rect
inRect f = vectorsToRect . f . rectToVectors

rectPos, rectSize :: Endo (Vector2 Int) -> Endo Rect
rectPos = inRect . first
rectSize = inRect . second

rectX, rectY, rectW, rectH :: Endo Int -> Endo Rect
rectX = rectPos . vector2first
rectY = rectPos . vector2second
rectW = rectSize . vector2first
rectH = rectSize . vector2second

makePosRect :: Vector2 Int -> Rect
makePosRect (Vector2 x y) = SDL.Rect x y 0 0

unionRects :: Rect -> Rect -> Rect
unionRects r1 r2 =
    let (tl1, br1) = vectorsToPVectors . rectToVectors $ r1
        (tl2, br2) = vectorsToPVectors . rectToVectors $ r2
    in vectorsToRect . pVectorsToVectors $
           ((liftA2 min tl1 tl2),
            (liftA2 max br1 br2))
