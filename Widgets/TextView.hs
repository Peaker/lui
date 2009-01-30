{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextView where

import qualified Widget
import Widget(Widget(..))
import qualified Graphics.UI.SDL as SDL
import qualified Draw

type New model = SDL.Color -> String -> Widget model

new :: New model
new textColor text =
    Widget
    {
      widgetDraw = \_ _ -> Draw.text textColor text
    , widgetSize = \_ _ -> Draw.textSize text
    , widgetGetKeymap = \_ -> Nothing
    }
