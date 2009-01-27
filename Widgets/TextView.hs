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

type NewTextView = SDL.Color -> String -> Widget.AnyWidgetState
new :: NewTextView
new textColor str =
    Widget.AnyWidgetState (TextView textColor str) State

instance Widget.Widget TextView State where
    getKeymap _ _ = Nothing

    size _ (TextView _ text) State = Draw.textSize text
    draw _ (TextView color text) State = Draw.text color text
