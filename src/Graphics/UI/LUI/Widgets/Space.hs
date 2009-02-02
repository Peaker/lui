{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Space where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import Graphics.UI.LUI.Draw(Size)
import Graphics.UI.HaskGame.Vector2(Vector2(..))

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
