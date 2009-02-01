{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Box where

import qualified Widget
import Widget(Widget)
import qualified Widgets.Grid as Grid
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified Data.Map as Map
import HaskGame.Color(Color)
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
      boxGridConvertor = convertor mutableToGridMutable gridMutableToMutable
      mutableToGridMutable = Grid.Mutable . maybeSwap . (,) 0 . mutableCursor
      gridMutableToMutable = Mutable . snd . maybeSwap . Grid.mutableCursor

type DelegatedImmutable model = (Color, (Immutable model))
type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable ^> aMutableCursor

delegatedMutable :: Bool -> Cursor -> DelegatedMutable
delegatedMutable startInside cursor =
    (FocusDelegator.Mutable startInside, Mutable cursor)

newDelegated :: Widget.New model (DelegatedImmutable model) DelegatedMutable
newDelegated immutableMaker acc model =
    let (focusColor, immutable) = immutableMaker model
        box = new (const immutable) (acc ^> FocusDelegator.aDelegatedMutable)
        focusDelegatorImmutable = FocusDelegator.Immutable
                                  "Go in" "Go out" box focusColor
    in FocusDelegator.new
           (const $ focusDelegatorImmutable)
           (acc ^> FocusDelegator.aFocusDelegatorMutable)
           model
