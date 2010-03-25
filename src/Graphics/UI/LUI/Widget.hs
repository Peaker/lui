{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Widget
    (DrawInfo(..), WidgetFuncs(..), Widget, New, rect, scale, drawText, textSize)
where

import Data.Accessor(Accessor)
import Data.Monoid(mconcat)
import Graphics.UI.LUI.Keymap(Keymap)

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Image, Any, (%%), Font)

rect :: Image Any
rect = Draw.convexPoly [(0, 0), (1, 0), (1, 1), (0, 1)]

scale :: Draw.R2 -> Draw.Affine
scale = uncurry Draw.scale

-- Draw.text puts text at (0, -descenderHeight)..(width, ascenderHeight)
-- drawText puts text at (0, 0)..(width, -totalHeight)
-- where descenderHeight = -fontDescender [which is negative].

textScale :: Draw.R2
textScale = (1/30, 1/20)

drawText :: Font -> String -> Image Any
drawText font str =
  mconcat [
    Draw.translate (0, -textHeight),
    scale textScale,
    Draw.translate (0, -Draw.fontDescender font)
    ] %%
  Draw.text font str
  where
    (_, textHeight) = textSize font str

liftTuple2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftTuple2 f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

textSize :: Font -> String -> Draw.R2
textSize font str = liftTuple2 (*) textScale $ Draw.textSize font str

data DrawInfo = DrawInfo { diHasFocus :: Bool }
  deriving (Eq, Ord, Show, Read)

--TODO: Replace Image/Size with SizedImage
--type SizedImage a = (Draw.R2, Image a)

data WidgetFuncs model = WidgetFuncs {
  -- The image should be neatly bound in (0, 0)..(width, -height)
    widgetImage :: DrawInfo -> Image Any
  , widgetSize :: DrawInfo -> Draw.R2
  , widgetGetKeymap :: Maybe (Keymap model)
}

type Widget model = model -> WidgetFuncs model

type New model mutable =
    Accessor model mutable -> Widget model
