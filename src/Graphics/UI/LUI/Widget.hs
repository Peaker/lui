{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Widget
    (DrawInfo(..),WidgetFuncs(..),Widget,New,rect,scale
    )
where

import Data.Accessor(Accessor)
import Graphics.UI.LUI.Keymap(Keymap)

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Image, Any)

rect :: Image Any
rect = Draw.convexPoly [(0, 0), (1, 0), (1, 1), (0, 1)]

scale :: Draw.R2 -> Draw.Affine
scale = uncurry Draw.scale


data DrawInfo = DrawInfo { diHasFocus :: Bool }
  deriving (Eq, Ord, Show, Read)

--TODO: Replace Image/Size with SizedImage
--type SizedImage a = (Draw.R2, Image a)

data WidgetFuncs model = WidgetFuncs {
    widgetImage :: DrawInfo -> Image Any
  , widgetSize :: DrawInfo -> Draw.R2
  , widgetGetKeymap :: Maybe (Keymap model)
}

type Widget model = model -> WidgetFuncs model

type New model mutable =
    Accessor model mutable -> Widget model
