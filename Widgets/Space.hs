{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Space where

import qualified Widget

type NewSpace = Widget.Size -> Widget.WidgetState Space State

data Space = Space
    {
      spaceSize :: Widget.Size
    }

data State = State
    {
    }

new :: NewSpace
new size = Widget.WidgetState (Space size) State

instance Widget.Widget Space State where
    getKeymap _ _ = Nothing
    draw _ (Space size) _ = return size
    size _ (Space size) _ = return size
