{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.KeysTable
    (defaultKeysColor
    ,defaultDescColor
    ,defaultSpaceWidth
    ,new
    ,newForWidget
    ,newBoxedWidget
    )
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Widgets.Box as Box
import qualified Graphics.UI.LUI.Widgets.Grid as Grid
import qualified Graphics.UI.LUI.Widgets.TextView as TextView
import qualified Graphics.UI.LUI.Widgets.Unfocusable as Unfocusable
import qualified Graphics.UI.LUI.Widgets.Space as Space
import qualified Graphics.UI.HaskGame.Key as Key
import Graphics.UI.LUI.Widget(Widget, widgetGetKeymap)

import Graphics.UI.HaskGame.Color(Color(..))
import Graphics.UI.HaskGame.Font(Font)

import qualified Data.Map as Map
import Control.Arrow(first, second)
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
      grid = Grid.new (3, Map.size handlers) gridItems $
             Grid.noAcc (error "Unfocusable grid should never use cursor")
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

newForWidget :: Font -> Font -> Widget model -> Widget model
newForWidget keysFont descFont widget model =
    let handlers = fromMaybe Map.empty . widgetGetKeymap $ widget model
    in new defaultKeysColor defaultDescColor defaultSpaceWidth
           keysFont descFont handlers model

newBoxedWidget :: Box.Orientation -> Int -> Font -> Font -> Widget model -> Widget model
newBoxedWidget orientation space keysFont descFont widget = box
    where
      box = Box.new orientation items $ Box.noAcc 0
      items = [Box.Item widget 0.5
              ,Box.Item (Space.newWH space space) 0
              ,Box.Item keysTable 0]
      keysTable = newForWidget keysFont descFont box
