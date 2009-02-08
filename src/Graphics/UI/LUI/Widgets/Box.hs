{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Box
    (Orientation(..)
    ,Item(..)
    ,Mutable(..)
    ,Cursor
    ,noAcc
    ,new
    ,aMutableCursor
    ,DelegatedMutable
    ,delegatedMutable
    ,aDelegatedMutableCursor
    ,newDelegated
    ,newDelegatedWith
    ) where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Widgets.Grid as Grid
import qualified Graphics.UI.LUI.Widgets.FocusDelegator as FocusDelegator
import Graphics.UI.LUI.Widget(Widget)
import Graphics.UI.LUI.Tuple(swap)
import Graphics.UI.LUI.Accessor(Accessor, reader, convertor, (^>))

import Graphics.UI.HaskGame.Color(Color(..))

import qualified Data.Map as Map

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
-- TODO: Auto-TH for this
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = convertor mutableCursor Mutable

noAcc :: Cursor -> Accessor model Mutable
noAcc cursor = reader . Mutable $ cursor

new :: Orientation -> [Item model] -> Widget.New model Mutable
new orientation items acc =
    Grid.new gridSize gridItems $ acc ^> boxGridConvertor
    where
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

newDelegatedWith :: Color -> Orientation -> [Item model] ->
                    Widget.New model DelegatedMutable
newDelegatedWith focusColor orientation items acc =
    let box = new orientation items $ acc ^> FocusDelegator.aDelegatedMutable
    in FocusDelegator.newWith focusColor "Go in" "Go out" box $
       acc ^> FocusDelegator.aFocusDelegatorMutable

newDelegated :: Orientation -> [Item model] -> Widget.New model DelegatedMutable
newDelegated = newDelegatedWith FocusDelegator.defaultFocusColor
