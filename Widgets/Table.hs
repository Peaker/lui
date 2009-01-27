{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
    -XExistentialQuantification
    -XRank2Types
  #-}

module Widgets.Table where

import qualified Widget
import qualified Widgets.Grid as Grid
import qualified Widgets.TextEdit as TextEdit
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as Map

data Table = Table
    {
      stateRows :: Rows
    }

data ColorString = ColorString SDL.Color String
type Rows = [[ColorString]]

data State = State
    {
    }

type NewTable = Rows -> Widget.AnyWidgetState

new :: NewTable
new rows = Widget.AnyWidgetState (Table rows) State

makeGrid :: NewTable
makeGrid rows = Grid.new (maximum . map length $ rows, length rows) (0, 0) $
                Map.fromList [((x, y),
                               Grid.Item (0.5, 0.5) $ TextEdit.newView color str)
                              | (y, row) <- zip [0..] rows
                              , (x, ColorString color str) <- zip [0..] row]

noFocus :: Widget.DrawInfo
noFocus = Widget.DrawInfo False

callOnGrid :: (forall w s. Widget.Widget w s => Widget.DrawInfo -> w -> s -> a) ->
              Table -> State -> a
callOnGrid func (Table rows) _ =
    makeGrid rows `Widget.onAnyWidgetState` func noFocus

instance Widget.Widget Table State where
    getKeymap _ _ = Map.empty
    draw _ = callOnGrid Widget.draw
    size _ = callOnGrid Widget.size