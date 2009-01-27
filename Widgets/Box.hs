{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Box where

import qualified Widget
import qualified Widgets.Grid as Grid
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as Map
import Control.Arrow(second)
import Func(result)
import Data.List(sortBy)
import Data.Ord(comparing)

data Orientation = Horizontal | Vertical

data Item = Item
    {
      itemAlignment :: Double
    , itemWidgetState :: Widget.AnyWidgetState
    }

type Cursor = Int

data Box = Box
    {
      boxOrientation :: Orientation
    }

data State = State
    {
      stateCursor :: Cursor
    , stateItems :: [Item]
    }

type NewBox = Orientation -> Cursor -> [Item] ->
              Widget.AnyWidgetState

new :: NewBox
new orientation cursor items = Widget.AnyWidgetState (Box orientation)
                                                     (State cursor items)

newDelegated :: SDL.Color -> Bool -> NewBox
newDelegated boxFocusColor startInItem orientation cursor items =
    FocusDelegator.new "Go in" "Go out" boxFocusColor startInItem $
                       new orientation cursor items

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

toOrientationTuple :: Orientation -> a -> a -> (a, a)
toOrientationTuple Vertical = (,)
toOrientationTuple Horizontal = curry swap
fromOrientationTuple :: Orientation -> (a, a) -> a
fromOrientationTuple Vertical = snd
fromOrientationTuple Horizontal = fst

boxToGridCall :: Box -> State -> (Grid.Grid -> Grid.State -> a) -> a
boxToGridCall (Box orientation) (State cursor items) func =
    func grid gridState
    where
      -- Requires a type signature because of the monomorphism
      -- restriction
      size = length items
      toTuple :: a -> a -> (a, a)
      toTuple = toOrientationTuple orientation
      grid = Grid.Grid $ toTuple 1 size
      gridItems = Map.fromList . map mapItem . zip [0..] $ items
      mapItem (index, Item alignment widgetState) =
          (toTuple 0 index,
           Grid.Item (swap $ toTuple 0 alignment) widgetState)
      gridState = Grid.State (toTuple 0 cursor) gridItems

gridStateToBoxState :: Box -> Grid.State -> State
gridStateToBoxState (Box orientation)
                    (Grid.State cursor items) =
    State (fromTuple cursor)
          (map (mapItem . snd) . sortBy (comparing fst) . Map.toList $ items)
    where
      -- Requires a type signature because of the monomorphism
      -- restriction
      fromTuple = fromOrientationTuple orientation
      mapItem (Grid.Item alignments widgetState) =
          Item (fromTuple . swap $ alignments) widgetState

instance Widget.Widget Box State where
    getKeymap box state = (Map.map . second . result) (gridStateToBoxState box) `fmap`
                          boxToGridCall box state Widget.getKeymap
    draw drawInfo box state = boxToGridCall box state $ Widget.draw drawInfo
    size drawInfo box state = boxToGridCall box state $ Widget.size drawInfo
