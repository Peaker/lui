{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Unfocusable where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

new :: Widget model -> Widget model
new childWidget model =
    let childWidgetFuncs = childWidget model
    in WidgetFuncs
    {
      widgetSize = \_ -> widgetSize childWidgetFuncs noFocusDrawInfo
    , widgetDraw = \_ -> widgetDraw childWidgetFuncs noFocusDrawInfo
    , widgetGetKeymap = Nothing
    }
