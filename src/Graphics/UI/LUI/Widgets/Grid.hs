{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Grid
    (Item(..)
    ,Mutable(..)
    ,Items
    ,Cursor
    ,noAcc
    ,new
    ,aMutableCursor
    ,DelegatedMutable
    ,delegatedMutable
    ,aDelegatedMutableCursor
    ,newDelegated
    ,newDelegatedWith
    )
where

import Control.Category((>>>))
import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import qualified Graphics.UI.LUI.Keymap as Keymap
import Graphics.UI.LUI.Keymap(Keymap, Doc)
import Graphics.UI.LUI.KeyGroup(KeyGroupName, showModKey, ModKey, noMods, ctrl)

import qualified Graphics.UI.LUI.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))

import Data.Accessor(Accessor, (^.), setVal)
import Data.Accessor.Simple(reader)
import Data.Accessor.Basic(fromWrapper)

import qualified Data.Map as Map
import Data.Map(Map, (!))

import Data.Maybe(listToMaybe)

import Control.Arrow(first, second)
import Data.List(transpose)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))

data Item model = Item {
  itemWidget :: Widget model,

  -- alignments are a number between 0..1 that
  -- represents where in the grid item's left..right
  -- and top..down the item should be in
  itemAlignments :: (Draw.R, Draw.R)
  }

type Items model = Map Cursor (Item model)
type Cursor = (Int, Int)
type Size = Cursor

data Mutable = Mutable {
  mutableCursor :: Cursor
  }

-- TODO: Auto-TH for this
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = fromWrapper Mutable mutableCursor

selectedItem :: Mutable -> Items model -> Maybe (Item model)
selectedItem (Mutable cursor) items = cursor `Map.lookup` items

gridRows :: Size -> Items model -> [[(Cursor, Maybe (Item model))]]
gridRows (sizex, sizey) items =
    [[((x,y), (x,y) `Map.lookup` items) | x <- [0..sizex-1]] | y <- [0..sizey-1]]

gridDrawInfo :: Mutable -> Cursor -> Widget.DrawInfo -> Widget.DrawInfo
gridDrawInfo (Mutable cursor) itemIndex (Widget.DrawInfo drawInfo) =
    Widget.DrawInfo (drawInfo && cursor==itemIndex)

getRowColumnSizes :: model -> Mutable -> Items model -> Size -> Widget.DrawInfo ->
                     ([Draw.R], [Draw.R])
getRowColumnSizes model mutable items size drawInfo = (rowHeights, columnWidths)
    where
      mapItems = map . map
      rowsSizes = mapItems itemWidgetSize (gridRows size items)
      itemWidgetSize (itemIndex, mItem) =
        case mItem of
          Nothing -> (0, 0)
          Just item -> widgetSize (itemWidget item model)
                                  (gridDrawInfo mutable itemIndex drawInfo)
      rowsHeights = mapItems snd rowsSizes
      rowsWidths =  mapItems fst rowsSizes

      rowHeights =   map maximum             $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths

itemSelectable :: model -> Item model -> Bool
itemSelectable model (Item widget _) = isJust . widgetGetKeymap . widget $ model

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

inRange :: Size -> Cursor -> Bool
inRange (w, h) (x, y) =
  isSorted [0, x, w-1] &&
  isSorted [0, y, h-1]

type Direction = Size -> Cursor -> [Cursor]

left :: Direction
right :: Direction
up :: Direction
down :: Direction
next :: Direction
prev :: Direction
left = nextCursors $ first (subtract 1)
right = nextCursors $ first (+1)
up = nextCursors $ second (subtract 1)
down = nextCursors $ second (+1)
next = scan first second (+1)
prev = scan first second (subtract 1)

nextCursors :: (Cursor -> Cursor) -> Direction
nextCursors forward size =
  takeWhile (`inRange` size) . drop 1 . iterate forward

type Endo a = a -> a
type CursorComponent = Endo Int -> Endo Cursor
scan :: CursorComponent -> CursorComponent -> Endo Int -> Direction
scan quick slow forward size cur = takeWhile inside . drop 1 . iterate smartNext $ cur
    where
      inside = inRange size
      smartNext c = if inside (quickNext c)
                    then quickNext c
                    else slowNext c
      quickNext = quick forward
      slowNext = quick (const 0) . slow forward

type GetSelectables model =
  Items model ->  -- ^ items
  model ->        -- ^ model
  [Cursor] ->     -- ^ cursors
  [Cursor]        -- ^ selectable forward cursors

filterSelectables :: GetSelectables model
filterSelectables items model  =
  filter (itemSelectable model . (items !))

type KeyGroupHandlers model = Map KeyGroupName (Doc, Map ModKey model)

makeHandlers :: ModKey -> Doc -> model -> KeyGroupHandlers model
makeHandlers modKey doc model =
  Map.singleton (showModKey modKey)
  (doc, Map.singleton modKey model)

directionHandlers :: [(Direction, model -> KeyGroupHandlers model)]
directionHandlers = [
  (left,  makeHandlers (noMods, GLUT.SpecialKey GLUT.KeyLeft)  "Move left"),
  (right, makeHandlers (noMods, GLUT.SpecialKey GLUT.KeyRight) "Move right"),
  (up,    makeHandlers (noMods, GLUT.SpecialKey GLUT.KeyUp)    "Move up"),
  (down,  makeHandlers (noMods, GLUT.SpecialKey GLUT.KeyDown)  "Move down"),
  (next,  makeHandlers (noMods, GLUT.Char '\t')   "Move to next"),
  (prev,  makeHandlers (ctrl,  GLUT.Char '\t')   "Move to prev")
  ]

keysMap :: Size -> Items model -> model -> Mutable -> Maybe (Keymap Mutable)
keysMap size items model (Mutable cursor) =
  -- If all item keymaps are Nothing, we get a Nothing. Otherwise, we
  -- get a Just . mconcat of all the keymaps
  mconcat allItemKeymaps
  where
    allItemKeymaps = map keymapOfDH directionHandlers
    keymapOfDH (direction, handlers) =
      listToMaybe .
      map (Keymap.make . handlers . Mutable) .
      filterSelectables items model .
      direction size $
      cursor

posSizes :: [Draw.R] -> [(Draw.R, Draw.R)]
posSizes sizes =
    let positions = scanl (+) 0 sizes
    in zip positions sizes

noAcc :: Cursor -> Accessor model Mutable
noAcc cursor = reader . Mutable $ cursor

new :: Size -> Items model -> Widget.New model Mutable
new size items acc model =
    WidgetFuncs {
      widgetImage = \drawInfo -> let
        (rowHeights, columnWidths) = rowColumnSizes drawInfo
        images =
          flip map (zip (posSizes rowHeights) rows) $
          \((ypos, height), row) ->
            flip map (zip (posSizes columnWidths) row) $
            \((xpos, width), (itemIndex, mItem)) ->
              case mItem of
                Nothing -> mempty
                Just item ->
                  let Item childWidget (ax, ay) = item
                      childDrawInfo = gridDrawInfo mutable itemIndex drawInfo
                      childWidgetFuncs = childWidget model
                      childImage = widgetImage childWidgetFuncs childDrawInfo
                      (w, h) = widgetSize childWidgetFuncs childDrawInfo
                      pos = ((xpos + ax * (width-w)),
                             ((ypos + ay * (height-h))))
                  in Draw.translate pos %% childImage
        in mconcat . concat $ images

    , widgetSize = gridSize . rowColumnSizes
    , widgetGetKeymap = ownKeymap `mappend` curChildKeymap
    }
  where
    ownKeymap = (fmap . fmap) applyToModel $ keysMap size items model mutable
    applyToModel newMutable = acc `setVal` newMutable $ model
    curChildKeymap = widgetGetKeymap . (`itemWidget` model) =<< selectedItem mutable items
    mutable = model ^. acc
    rows = gridRows size items
    rowColumnSizes drawInfo = getRowColumnSizes model mutable items size drawInfo
    gridSize (rowHeights, columnWidths) = (sum columnWidths, sum rowHeights)

type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable >>> aMutableCursor

delegatedMutable :: Bool -> Cursor -> DelegatedMutable
delegatedMutable startInside cursor =
    (FocusDelegator.Mutable startInside, Mutable cursor)

newDelegatedWith :: Draw.Color -> Size -> Items model -> Widget.New model DelegatedMutable
newDelegatedWith focusColor size items acc =
    let grid = new size items $ acc >>> FocusDelegator.aDelegatedMutable
    in FocusDelegator.newWith focusColor "Go in" "Go out" grid $
           acc >>> FocusDelegator.aFocusDelegatorMutable

newDelegated :: Cursor -> Items model -> Widget.New model DelegatedMutable
newDelegated = newDelegatedWith FocusDelegator.defaultFocusColor
