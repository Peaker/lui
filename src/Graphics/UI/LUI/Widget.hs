{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Widget
    (DrawInfo(..), WidgetFuncs(..), Widget, New, rect, scale, drawText, textSize)
where

import Data.Accessor(Accessor)
import Data.Monoid(mappend)
import Graphics.UI.LUI.Keymap(Keymap)

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Image, Any, (%%), Font)

rect :: Image Any
rect = Draw.convexPoly [(0, 0), (1, 0), (1, 1), (0, 1)]

scale :: Draw.R2 -> Draw.Affine
scale = uncurry Draw.scale

-- Draw.text puts text at (0, -descenderHeight)..(width, ascenderHeight)
-- drawText puts text at (0, 0)..(width, totalHeight)
-- where descenderHeight = -fontDescender [which is negative].

drawText :: Font -> String -> Image Any
drawText font str = scale ((1/10), (1/10)) `mappend`
                    Draw.translate (0, -Draw.fontDescender font)
                    %% Draw.text font str

textSize :: Font -> String -> Draw.R2
textSize font str = (Draw.textWidth font str / 10,
                     Draw.fontHeight font / 10)

data DrawInfo = DrawInfo { diHasFocus :: Bool }
  deriving (Eq, Ord, Show, Read)

--TODO: Replace Image/Size with SizedImage
--type SizedImage a = (Draw.R2, Image a)

data WidgetFuncs model = WidgetFuncs {
  -- The image should be neatly bound in (0, 0)..widgetSize
    widgetImage :: DrawInfo -> Image Any
  , widgetSize :: DrawInfo -> Draw.R2
  , widgetGetKeymap :: Maybe (Keymap model)
}

type Widget model = model -> WidgetFuncs model

type New model mutable =
    Accessor model mutable -> Widget model
