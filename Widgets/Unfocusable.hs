{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Unfocusable where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

data Immutable model = Immutable
    {
      immutableChildWidget :: Widget model
    }

new :: Widget.NewImmutable model (Immutable model)
new immutableMaker model =
    let Immutable childWidget = immutableMaker model
        childWidgetFuncs = childWidget model
    in WidgetFuncs
    {
      widgetSize = \_ -> widgetSize childWidgetFuncs noFocusDrawInfo
    , widgetDraw = \_ -> widgetDraw childWidgetFuncs noFocusDrawInfo
    , widgetGetKeymap = Nothing
    }
