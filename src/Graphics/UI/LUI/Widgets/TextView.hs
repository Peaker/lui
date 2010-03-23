{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.TextView(new)
where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import qualified Graphics.DrawingCombinators as Draw

new :: Draw.Color -> Draw.Font -> String -> Widget model
new textColor font text =
    const $
    WidgetFuncs {
      widgetImage = const $ textColor `Draw.tint` Widget.drawText font text,
      widgetSize = const $ Widget.textSize font text,
      widgetGetKeymap = Nothing
    }
