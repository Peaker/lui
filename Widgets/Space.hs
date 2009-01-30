{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Space where

import qualified Widget
import Widget(Widget(..))
import Draw(Size)

type New model = Size -> Widget model

new :: New model
new size =
    Widget
    {
      widgetGetKeymap = \_ -> Nothing
    , widgetDraw = \_ _ -> return size
    , widgetSize = \_ _ -> return size
    }
