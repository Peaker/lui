{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Box where

import qualified Widget
import Widget(Widget(..))
import qualified Widgets.Grid as Grid
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as Map
import Tuple(swap)
import Accessor(accessor, (^>), afirst, asecond)

data Orientation = Horizontal | Vertical

data Item model = Item
    {
      itemChildWidget :: Widget model

    -- See comment about Grid.Item alignments
    , itemAlignment :: Double
    }

type Cursor = Int

data Mutable = Mutable
    {
      mutableCursor :: Cursor
    }

-- This type exists for symmetry/similarity with Grid's Items
type Items model = [Item model]

type New model mutable = Orientation -> Items model -> Widget.New model mutable

new :: New model Mutable
new orientation items boxAccessor =
    Grid.new gridSize
             gridItems $
             boxAccessor ^> boxGridConvertor
    where
      gridSize = (maybeSwap (1, length items))
      gridItems = (Map.fromList $
                   [(maybeSwap (0, i),
                     Grid.Item childWidget . maybeSwap $ (0, alignment))
                    | (i, Item childWidget alignment) <- zip [0..] items])
      maybeSwap = case orientation of
                    Vertical -> id
                    Horizontal -> swap
      boxGridConvertor = accessor mutableToGridMutable $
                         const . gridMutableToMutable
      mutableToGridMutable = Grid.Mutable . maybeSwap . (,) 0 . mutableCursor
      gridMutableToMutable = Mutable . snd . maybeSwap . Grid.mutableCursor

newDelegated :: SDL.Color ->
                New model (FocusDelegator.Mutable, Mutable)
newDelegated focusColor orientation items boxAccessor =
    let box = new orientation items $ boxAccessor ^> asecond
    in FocusDelegator.new "Go in" "Go out" focusColor box $
       boxAccessor ^> afirst
