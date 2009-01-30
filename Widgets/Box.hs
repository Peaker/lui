{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Box where

import qualified Widget
import Widget(Widget)
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

data Immutable model = Immutable
    {
      immutableOrientation :: Orientation
    , immutableItems :: Items model
    }

data Mutable = Mutable
    {
      mutableCursor :: Cursor
    }

-- This type exists for symmetry/similarity with Grid's Items
type Items model = [Item model]

new :: Widget.New model (Immutable model) Mutable
new immutableMaker acc model =
    Grid.new (const $ Grid.Immutable gridSize gridItems)
             (acc ^> boxGridConvertor)
             model
    where
      Immutable orientation items = immutableMaker model
          
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

newDelegated :: Widget.New model (SDL.Color, (Immutable model))
                                 (FocusDelegator.Mutable, Mutable)
newDelegated immutableMaker acc model =
    let (focusColor, immutable) = immutableMaker model
        box = new (const immutable) (acc ^> asecond)
        focusDelegatorImmutable = FocusDelegator.Immutable
                                  "Go in" "Go out" box focusColor
    in FocusDelegator.new
           (const $ focusDelegatorImmutable)
           (acc ^> afirst) model
