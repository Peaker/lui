{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Space where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))
import HaskGame.Vector2(Vector2(..))
import Draw(Size)

new :: Size -> Widget model
new size =
    const $
    WidgetFuncs
    {
      widgetGetKeymap = Nothing
    , widgetDraw = \_ -> return size
    , widgetSize = \_ -> return size
    }

newWH :: Int -> Int -> Widget model
newWH w h = new $ Vector2 w h

newW, newH :: Int -> Widget model
newW w = newWH w 0
newH h = newWH 0 h
