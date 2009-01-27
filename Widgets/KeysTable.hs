{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.KeysTable where

import qualified Widget
import qualified MySDLKey
import qualified Graphics.UI.SDL as SDL
import qualified Widgets.Grid as Grid
import qualified Widgets.TextView as TextView
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Data.List(sort)
import Control.Arrow(first, second)

gItem :: Widget.AnyWidgetState -> Grid.Item
gItem = Grid.Item (0.5, 0.5)

grid :: SDL.Color -> SDL.Color ->
        Widget.AnyWidgetState -> Widget.WidgetState Grid.Grid Grid.State
grid keysColor descColor (Widget.AnyWidgetState widget state) =
    Widget.WidgetState (Grid.Grid (2, Map.size keyMap)) (Grid.State (0,0) items)
    where
      keyMap = fromMaybe Map.empty $ Widget.getKeymap widget state
      items = Map.fromList . concat $
              [[((0, y), gItem . Widget.upCast $ keyGroupTextView),
                ((1, y), gItem . Widget.upCast $ descTextView)]
               | (y, (keyGroup, desc)) <-
                   zip [0..] . sort . (map . first) snd . (map . second) fst $
                       Map.assocs keyMap,
              let keyGroupTextView = TextView.new keysColor $
                                     MySDLKey.keyGroupName keyGroup
                  descTextView = TextView.new descColor desc]
