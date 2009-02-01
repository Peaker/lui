{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Space where

import qualified Widget
import Widget(WidgetFuncs(..))
import HaskGame.Vector2(Vector2(..))
import Draw(Size)

data Immutable = Immutable
    {
      immutableSize :: Size
    }

imm :: Int -> Int -> Immutable
imm x y = Immutable $ Vector2 x y

new :: Widget.NewImmutable model Immutable
new immutableMaker model =
    let Immutable size = immutableMaker model
    in WidgetFuncs
    {
      widgetGetKeymap = Nothing
    , widgetDraw = \_ -> return size
    , widgetSize = \_ -> return size
    }
