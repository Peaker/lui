{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Space(new, newWH, newW, newH)
where

import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import qualified Graphics.DrawingCombinators as Draw

import Data.Monoid(mempty)

new :: Draw.R2 -> Widget model
new size =
    const $
    WidgetFuncs
    {
      widgetGetKeymap = Nothing
    , widgetImage = const mempty
    , widgetSize = const size
    }

newWH :: Draw.R -> Draw.R -> Widget model
newWH w h = new $ (w, h)

newW :: Draw.R -> Widget model
newH :: Draw.R -> Widget model
newW w = newWH w 0
newH h = newWH 0 h
