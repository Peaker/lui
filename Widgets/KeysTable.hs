{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.KeysTable where

import qualified Widget
import Widget(Widget(..))
import qualified MySDLKey
import qualified Graphics.UI.SDL as SDL
import qualified Widgets.Grid as Grid
import qualified Widgets.TextView as TextView
import qualified Widgets.Unfocusable as Unfocusable
import qualified Data.Map as Map
import Data.List(sort)
import Control.Arrow(first, second)
import Accessor(reader)

gItem :: Widget model -> Grid.Item model
gItem = flip Grid.Item (0.5, 0.5)

type New model a = Widget.ActionHandlers a -> Widget model

keyBindings :: Widget.ActionHandlers a -> [(MySDLKey.KeyGroup, String)]
keyBindings = sort .
              (map . first) snd .
              (map . second) fst .
              Map.assocs

new :: SDL.Color -> SDL.Color -> New model a
new keysColor descColor handlers =
    Unfocusable.new $
    Grid.new (2, Map.size handlers) gridItems gridAccessor
    where
      gridItems =
          Map.fromList . concat $
          [[((0, y), gItem keyGroupTextView),
            ((1, y), gItem descTextView)]
           | (y, (keyGroup, desc)) <- zip [0..] . keyBindings $ handlers
           , let keyGroupTextView = TextView.new keysColor $
                                    MySDLKey.keyGroupName keyGroup
                 descTextView = TextView.new descColor desc]
      gridAccessor = reader $ Grid.Mutable (0,0)
