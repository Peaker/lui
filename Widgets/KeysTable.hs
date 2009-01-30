{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.KeysTable where

import qualified Widget
import Widget(Widget)
import qualified MySDLKey
import qualified Graphics.UI.SDL as SDL
import qualified Widgets.Grid as Grid
import qualified Widgets.TextView as TextView
import qualified Widgets.Unfocusable as Unfocusable
import qualified Data.Map as Map
import Data.List(sort)
import Control.Arrow(first, second)
import Accessor(reader)

data Immutable a = Immutable
    {
      immutableKeysColor :: SDL.Color
    , immutableDescColor :: SDL.Color
    , immutableHandlers :: Widget.ActionHandlers a
    }

gItem :: Widget model -> Grid.Item model
gItem = flip Grid.Item (0.5, 0.5)

keyBindings :: Widget.ActionHandlers a -> [(MySDLKey.KeyGroup, String)]
keyBindings = sort .
              (map . first) snd .
              (map . second) fst .
              Map.assocs

new :: Widget.NewImmutable model (Immutable a)
new immutableMaker model =
    Unfocusable.new (const $ Unfocusable.Immutable grid) model
    where
      Immutable keysColor descColor handlers = immutableMaker model
      grid = Grid.new (const $ Grid.Immutable (2, Map.size handlers) gridItems) gridAccessor
      gridItems =
          Map.fromList . concat $
          [[((0, y), gItem keyGroupTextView),
            ((1, y), gItem descTextView)]
           | (y, (keyGroup, desc)) <- zip [0..] . keyBindings $ handlers
           , let keyGroupTextView =
                     TextView.new (const $
                                   TextView.Immutable keysColor
                                   $ MySDLKey.keyGroupName keyGroup)
                 descTextView =
                     TextView.new (const $
                                   TextView.Immutable descColor desc)
          ]
      gridAccessor = reader $ Grid.Mutable (0,0)
