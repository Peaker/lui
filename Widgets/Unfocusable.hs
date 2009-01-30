{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Unfocusable where

import qualified Widget
import Widget(Widget(..))

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

type New model = Widget model -> Widget model

new :: New model
new childWidget =
    Widget
    {
      widgetSize = \_ model -> widgetSize childWidget noFocusDrawInfo model
    , widgetDraw = \_ model -> widgetDraw childWidget noFocusDrawInfo model
    , widgetGetKeymap = \_ -> Nothing
    }
