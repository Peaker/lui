{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Space(new, newWH, newW, newH)
where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import Graphics.UI.HaskGame(Size)
import Graphics.UI.HaskGame.Vector2(Vector2(..))

import Data.Monoid(mempty)

new :: Size -> Widget model
new size =
    const $
    WidgetFuncs
    {
      widgetGetKeymap = Nothing
    , widgetImage = const mempty
    , widgetSize = const size
    }

newWH :: Int -> Int -> Widget model
newWH w h = new $ Vector2 w h

newW, newH :: Int -> Widget model
newW w = newWH w 0
newH h = newWH 0 h
