{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.TextView where

import Widget(Widget, WidgetFuncs(..))
import qualified Widget
import qualified Draw
import HaskGame.Color(Color)

data Immutable = Immutable
    {
      immutableTextColor :: Color
    , immutableFont :: Draw.Font
    , immutableText :: String
    }

type New model immutable =
    (model -> immutable) -> Widget model

new :: Widget.NewImmutable model Immutable
new immutableMaker model =
    let Immutable textColor textSize text = immutableMaker model
    in WidgetFuncs
    {
      widgetDraw = \_ -> Draw.text textColor textSize text
    , widgetSize = \_ -> Draw.textSize textSize text
    , widgetGetKeymap = Nothing
    }
