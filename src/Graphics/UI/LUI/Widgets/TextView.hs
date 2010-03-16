{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.TextView(new)
where

import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import qualified Graphics.DrawingCombinators as Draw

new :: Draw.Color -> Draw.Font -> String -> Widget model
new textColor font text =
    const $
    WidgetFuncs
    {
      widgetImage = const $ textColor `Draw.tint` Draw.text font text
    , widgetSize = const $ Draw.textSize font text
    , widgetGetKeymap = Nothing
    }
