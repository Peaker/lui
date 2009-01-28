{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Unfocusable where

import qualified Widget

data Unfocusable = Unfocusable {
     unfocusableWidget :: Widget.AnyWidgetState
}

data State = State {
}

type New w s = Widget.AnyWidgetState -> Widget.WidgetState w s

new :: New Unfocusable State
new widget = Widget.WidgetState (Unfocusable widget) State

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

instance Widget.Widget Unfocusable State where
    getKeymap _ _ = Nothing
    size _ (Unfocusable widget) _ = Widget.onAnyWidgetState widget $ Widget.size noFocusDrawInfo
    draw _ (Unfocusable widget) _ = Widget.onAnyWidgetState widget $ Widget.draw noFocusDrawInfo
