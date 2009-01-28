{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Space where

import qualified Widget

data Space = Space
    {
      spaceSize :: Widget.Size
    }

data State = State
    {
    }

type SpaceState = Widget.WidgetState Space State
type New w = Widget.Size -> w

new :: New SpaceState
new size = Widget.WidgetState (Space size) State

instance Widget.Widget Space State where
    getKeymap _ _ = Nothing
    draw _ (Space size) _ = return size
    size _ (Space size) _ = return size
