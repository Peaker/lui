{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Space where

import qualified Widget
import Widget(WidgetFuncs(..))
import Draw(Size)

data Immutable = Immutable
    {
      immutableSize :: Size
    }

new :: Widget.NewImmutable model Immutable
new immutableMaker model =
    let Immutable size = immutableMaker model
    in WidgetFuncs
    {
      widgetGetKeymap = Nothing
    , widgetDraw = \_ -> return size
    , widgetSize = \_ -> return size
    }
