{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextView where

import qualified Widget
import qualified Graphics.UI.SDL as SDL
import qualified Draw

data TextView = TextView {
      textViewColor :: SDL.Color
    , textViewText :: String
}

data State = State {
}

type TextViewState = Widget.WidgetState TextView State
type New w = SDL.Color -> String -> w

new :: New TextViewState
new textColor str =
    Widget.WidgetState (TextView textColor str) State

instance Widget.Widget TextView State where
    getKeymap _ _ = Nothing

    size _ (TextView _ text) State = Draw.textSize text
    draw _ (TextView color text) State = Draw.text color text
