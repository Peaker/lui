{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.KeysTable where

import qualified Widget
import Widget(Widget)
import qualified HaskGame.Key as Key
import qualified HaskGame.Color as Color
import qualified Widgets.Grid as Grid
import qualified Widgets.TextView as TextView
import qualified Widgets.Unfocusable as Unfocusable
import qualified Draw
import qualified Data.Map as Map
import Data.List(sort)
import Control.Arrow(first, second)
import Accessor(reader)

data Immutable a = Immutable
    {
      immutableKeysColor :: Color.Color
    , immutableKeysFont :: Draw.Font
    , immutableDescColor :: Color.Color
    , immutableDescFont :: Draw.Font
    , immutableHandlers :: Widget.ActionHandlers a
    }

gItem :: Widget model -> Grid.Item model
gItem = flip Grid.Item (0, 0.5)

keyBindings :: Widget.ActionHandlers a -> [(Key.KeyGroup, String)]
keyBindings = sort .
              (map . first) snd .
              (map . second) fst .
              Map.assocs

new :: Widget.NewImmutable model (Immutable a)
new immutableMaker model =
    Unfocusable.new (const $ Unfocusable.Immutable grid) model
    where
      Immutable keysColor keysFont descColor descFont handlers = immutableMaker model
      grid = Grid.new (const $ Grid.Immutable (2, Map.size handlers) gridItems) gridAccessor
      gridItems =
          Map.fromList . concat $
          [[((0, y), gItem keyGroupTextView),
            ((1, y), gItem descTextView)]
           | (y, (keyGroup, desc)) <- zip [0..] . keyBindings $ handlers
           , let keyGroupTextView =
                     TextView.new (const . TextView.Immutable keysColor keysFont $
                                   Key.keyGroupName keyGroup)
                 descTextView =
                     TextView.new (const $
                                   TextView.Immutable descColor descFont desc)
          ]
      gridAccessor = reader $ Grid.Mutable (0,0)
