{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Unfocusable where

import qualified Widget

data Widget.Widget w s => Unfocusable w s = Unfocusable {
     unfocusableWidget :: Widget.WidgetState w s
}
data State w s = State {
}

type UnfocusableState w s = Widget.WidgetState (Unfocusable w s) (State w s)
type New w s r = Widget.WidgetState w s -> r

new :: Widget.Widget w s => New w s (UnfocusableState w s)
new widget = Widget.WidgetState (Unfocusable widget) State

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

instance Widget.Widget w s => Widget.Widget (Unfocusable w s) (State w s) where
    getKeymap _ _ = Nothing
    size _ (Unfocusable widget) _ = Widget.onWidgetState widget $ Widget.size noFocusDrawInfo
    draw _ (Unfocusable widget) _ = Widget.onWidgetState widget $ Widget.draw noFocusDrawInfo
