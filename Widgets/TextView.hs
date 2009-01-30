{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextView where

import qualified Widget
import Widget(Widget(..))
import qualified Graphics.UI.SDL as SDL
import qualified Draw

type New = SDL.Color -> String -> Widget ()

new :: New
new textColor text =
    Widget
    {
      widgetDraw = \_ () -> Draw.text textColor text
    , widgetSize = \_ () -> Draw.textSize text
    , widgetGetKeymap = \_ -> Nothing
    }
