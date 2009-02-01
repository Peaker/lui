{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Proxy where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

data Immutable model = Immutable
    {
      immutableChildWidget :: Widget model
    }

imm :: Widget model -> Immutable model
imm = Immutable

new :: Widget.NewImmutable model (Immutable model)
new immutableMaker model =
    let Immutable childWidget = immutableMaker model
        childWidgetFuncs = childWidget model
    in WidgetFuncs
    {
      widgetSize = widgetSize childWidgetFuncs
    , widgetDraw = widgetDraw childWidgetFuncs
    , widgetGetKeymap = widgetGetKeymap childWidgetFuncs
    }
