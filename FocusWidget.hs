{-# OPTIONS_GHC -Wall -O2
  #-}

module FocusWidget where

import qualified Widget

class HasChildren w s where
    currentWidget :: w -> s -> Widget.AnyWidgetState

