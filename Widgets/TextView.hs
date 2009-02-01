{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.TextView where

import Widget(Widget, WidgetFuncs(..))
import qualified Widget
import qualified Draw
import HaskGame.Font(Font)
import HaskGame.Color(Color)

new :: Color -> Font -> String -> Widget model
new textColor textSize text =
    const $
    WidgetFuncs
    {
      widgetDraw = \_ -> Draw.text textColor textSize text
    , widgetSize = \_ -> Draw.textSize textSize text
    , widgetGetKeymap = Nothing
    }
