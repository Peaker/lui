{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Box where

import qualified Widget
import Widget(Widget)
import qualified Widgets.Grid as Grid
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified Data.Map as Map
import Tuple(swap)
import Accessor(Accessor, convertor, (^>))

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

imm :: Orientation -> Items model -> Immutable model
imm = Immutable

data Mutable = Mutable
    {
      mutableCursor :: Cursor
    }
-- TODO: Auto-TH for this
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = convertor mutableCursor Mutable

-- This type exists for symmetry/similarity with Grid's Items
type Items model = [Item model]

new :: Widget.New model (Immutable model) Mutable
new acc immutableMaker model =
    Grid.new (acc ^> boxGridConvertor)
            (const $ Grid.imm gridSize gridItems)
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
      boxGridConvertor = convertor mutableToGridMutable gridMutableToMutable
      mutableToGridMutable = Grid.Mutable . maybeSwap . (,) 0 . mutableCursor
      gridMutableToMutable = Mutable . snd . maybeSwap . Grid.mutableCursor

type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable ^> aMutableCursor

delegatedMutable :: Bool -> Cursor -> DelegatedMutable
delegatedMutable startInside cursor =
    (FocusDelegator.Mutable startInside, Mutable cursor)

newDelegatedWithFocusableArgs ::
    Widget.New model (Widget model -> FocusDelegator.Immutable model,
                      Immutable model) DelegatedMutable
newDelegatedWithFocusableArgs acc immutableMaker model =
    let (focusableImmutableMaker, immutable) = immutableMaker model
        box = new (acc ^> FocusDelegator.aDelegatedMutable) . const $ immutable
    in FocusDelegator.new
           (acc ^> FocusDelegator.aFocusDelegatorMutable)
           (const $ focusableImmutableMaker box)
           model

newDelegated :: Widget.New model (Immutable model) DelegatedMutable
newDelegated acc immutableMaker model =
    newDelegatedWithFocusableArgs
      acc
      (const (FocusDelegator.imm "Go in" "Go out",
              immutableMaker model))
      model
