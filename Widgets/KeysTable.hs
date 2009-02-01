{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.KeysTable where

import qualified Widget
import Widget(Widget, widgetGetKeymap)
import qualified HaskGame.Key as Key
import HaskGame.Color(Color(..))
import HaskGame.Font(Font)
import qualified Widgets.Grid as Grid
import qualified Widgets.TextView as TextView
import qualified Widgets.Unfocusable as Unfocusable
import qualified Widgets.Space as Space
import qualified Data.Map as Map
import Control.Arrow(first, second)
import Accessor(reader)
import Data.List(sort)
import Data.Maybe(fromMaybe)

-- Defaults:
defaultKeysColor, defaultDescColor :: Color
defaultKeysColor = Color 255 0 0
defaultDescColor = Color 0 0 255
defaultSpaceWidth :: Int
defaultSpaceWidth = 10

gItem :: Widget model -> Grid.Item model
gItem = flip Grid.Item (0, 0.5)

keyBindings :: Widget.ActionHandlers model -> [(Key.KeyGroup, String)]
keyBindings = sort .
              (map . first) snd .
              (map . second) fst .
              Map.assocs

new :: Color -> Color -> Int -> Font -> Font -> Widget.ActionHandlers model ->
       Widget model
new keysColor descColor spaceWidth keysFont descFont handlers = Unfocusable.new grid
    where
      grid = Grid.new (3, Map.size handlers) gridItems gridAccessor
      space = Space.newW spaceWidth
      gridItems =
          Map.fromList . concat $
          [[((0, y), gItem keyGroupTextView),
            ((1, y), gItem space),
            ((2, y), gItem descTextView)]
           | (y, (keyGroup, desc)) <- zip [0..] . keyBindings $ handlers
           , let keyGroupTextView =
                     TextView.new keysColor keysFont $ Key.keyGroupName keyGroup
                 descTextView =
                     TextView.new descColor descFont desc
          ]
      gridAccessor = reader $ Grid.Mutable (0,0)

newForWidget :: Font -> Font -> Widget model -> Widget model
newForWidget keysFont descFont widget model =
    let handlers = fromMaybe Map.empty . widgetGetKeymap $ widget model
    in new defaultKeysColor defaultDescColor defaultSpaceWidth
           keysFont descFont handlers model
