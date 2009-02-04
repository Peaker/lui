{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Grid
    (Item(..)
    ,Mutable(..)
    ,Items
    ,Cursor
    ,new
    ,aMutableCursor
    ,DelegatedMutable
    ,delegatedMutable
    ,aDelegatedMutableCursor
    ,newDelegated
    ,newDelegatedWith
    )
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.LUI.Image as Image

import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import Graphics.UI.LUI.Func((~>), result)
import Graphics.UI.LUI.List(isSorted)
import Graphics.UI.LUI.Tuple(swap)
import Graphics.UI.LUI.Accessor(Accessor, convertor, (^.), (^>), write)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.HaskGame.Key as Key
import Graphics.UI.HaskGame.Key(asKeyGroup, noMods, shift)
import Graphics.UI.HaskGame.Color(Color)
import Graphics.UI.HaskGame.Vector2(Vector2(..)
                                   ,vector2fst,vector2snd)

import qualified Data.Map as Map
import Control.Arrow(second, (***))
import Data.List(transpose)
import Data.Maybe(isJust, isNothing, fromMaybe)
import Data.Monoid(mempty, mconcat)

data Item model = Item
    {
      itemWidget :: Widget model

      -- alignments are a number between 0..1 that
      -- represents where in the grid item's left..right
      -- and top..down the item should be in
    , itemAlignments :: (Double, Double)
    }

type Items model = Map.Map Cursor (Item model)
type Cursor = (Int, Int)

data Mutable = Mutable
    {
      mutableCursor :: Cursor
    }
-- TODO: Auto-TH for this
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = convertor mutableCursor Mutable

selectedItem :: Mutable -> Items model -> Maybe (Item model)
selectedItem (Mutable cursor) items = cursor `Map.lookup` items

gridRows :: Cursor -> Items model -> [[(Cursor, Maybe (Item model))]]
gridRows (sizex, sizey) items =
    [[((x,y), (x,y) `Map.lookup` items) | x <- [0..sizex-1]] | y <- [0..sizey-1]]

gridDrawInfo :: Mutable -> Cursor -> Widget.DrawInfo -> Widget.DrawInfo
gridDrawInfo (Mutable cursor) itemIndex (Widget.DrawInfo drawInfo) =
    Widget.DrawInfo (drawInfo && cursor==itemIndex)

getRowColumnSizes :: model -> Mutable -> Items model -> Cursor -> Widget.DrawInfo ->
                     ([Int], [Int])
getRowColumnSizes model mutable items size drawInfo = (rowHeights, columnWidths)
    where
      mapItems = map . map
      rowsSizes = mapItems itemWidgetSize (gridRows size items)
      itemWidgetSize (itemIndex, mItem) =
        case mItem of
          Nothing -> Vector2 0 0
          Just item -> widgetSize (itemWidget item model)
                                  (gridDrawInfo mutable itemIndex drawInfo)
      rowsHeights = mapItems vector2snd rowsSizes
      rowsWidths =  mapItems vector2fst rowsSizes

      rowHeights =   map maximum             $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths

mutableCursorApply :: (Cursor -> Cursor) -> Mutable -> Mutable
mutableCursorApply func (Mutable oldCursor) = Mutable $ func oldCursor

mutableMoveTo :: (Int, Int) -> Mutable -> Mutable
mutableMoveTo newCursor = mutableCursorApply (const newCursor)

itemSelectable :: model -> Item model -> Bool
itemSelectable model (Item widget _) =
    isJust . widgetGetKeymap $ widget model

getSelectables :: ((Int, Int) -> (Int, Int)) ->
                  (Int -> Int) ->
                  Cursor -> model -> Mutable -> Items model ->
                  [(Int, Int)]
getSelectables toSwap cursorFunc size model (Mutable oldCursor) items =
    let (sizeA,_) = toSwap size
        (oldA,b) = toSwap oldCursor
        nexts = takeWhile (\a -> isSorted [0, a, sizeA-1]) .
                iterate cursorFunc $ oldA
        coor a = toSwap (a, b)
        itemAt a = (coor a, items Map.! coor a)
    in map fst . filter (itemSelectable model . snd) . map itemAt . drop 1 $ nexts

getSelectablesX, getSelectablesY :: (Int -> Int) ->
                                    Cursor -> model -> Mutable -> Items model ->
                                    [(Int, Int)]
getSelectablesX = getSelectables id
getSelectablesY = getSelectables swap

getSelectablesXY :: [Int] -> (Int -> Int) ->
                    Cursor -> model -> Mutable -> Items model ->
                    [(Int, Int)]
getSelectablesXY xrange cursorFunc size model (Mutable oldCursor) items =
    let (sizeX,sizeY) = size
        (oldX,oldY) = oldCursor
        xnexts = drop 1 . takeWhile (\x -> isSorted [0, x, sizeX-1]) .
                 iterate cursorFunc $ oldX
        ynexts = drop 1 . takeWhile (\y -> isSorted [0, y, sizeY-1]) .
                 iterate cursorFunc $ oldY
        nexts = map (flip (,) oldY) xnexts ++
                [(x, y) | y <- ynexts, x <- xrange]
        itemAt cursor = (cursor, items Map.! cursor)
    in map fst . filter (itemSelectable model . snd) . map itemAt $ nexts

keysMap :: Cursor -> model -> Mutable -> Items model -> Widget.ActionHandlers Mutable
keysMap size@(sizeX, _) model mutable items =
    Map.fromList $
       map (((,) Widget.KeyDown) ***
            second const) . concat $
           [let opts = axis size model mutable items
            in cond opts (key, (desc, head opts `mutableMoveTo` mutable))
            | (key, axis, desc) <-
                [(asKeyGroup noMods SDL.SDLK_LEFT,
                  getSelectablesX (subtract 1), "Move left")
                ,(asKeyGroup noMods SDL.SDLK_RIGHT,
                  getSelectablesX (+1), "Move right")
                ,(asKeyGroup noMods SDL.SDLK_UP,
                  getSelectablesY (subtract 1), "Move up")
                ,(asKeyGroup noMods SDL.SDLK_DOWN,
                  getSelectablesY (+1), "Move down")
                ,(asKeyGroup noMods SDL.SDLK_TAB,
                  getSelectablesXY [0..sizeX-1] (+1), "Move to next")
                ,(asKeyGroup shift SDL.SDLK_TAB,
                  getSelectablesXY [sizeX-1,sizeX-2..0] (subtract 1), "Move to prev")
                ]
           ]
    where
      cond p i = if not . null $ p then [i] else []

inFrac :: (Integral a, RealFrac b) => (b -> b) -> a -> a
inFrac = fromIntegral ~> floor

posSizes :: [Int] -> [(Int, Int)]
posSizes sizes =
    let positions = scanl (+) 0 sizes
    in zip positions sizes

new :: Cursor -> Items model -> Widget.New model Mutable
new size items acc model =
    let mutable = model ^. acc
        rows = gridRows size items
        rowColumnSizes drawInfo = getRowColumnSizes model mutable items size drawInfo
    in WidgetFuncs
    {
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
                        Vector2 w h = widgetSize childWidgetFuncs childDrawInfo
                        pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                      (ypos + inFrac (*ay) (height-h))
                    in Image.move pos childImage
        in mconcat . concat $ images

    , widgetSize =
        \drawInfo ->
            let (rowHeights, columnWidths) = rowColumnSizes drawInfo
            in Vector2 (sum columnWidths) (sum rowHeights)

    , widgetGetKeymap =
      if all isNothing .
         map (widgetGetKeymap . ($model) . itemWidget) .
         Map.elems $ items
      then Nothing
      else Just $
        let childKeys = fromMaybe Map.empty $ do
                          Item childWidget _ <- selectedItem mutable items
                          widgetGetKeymap $ childWidget model
            applyToModel newMutable = acc `write` newMutable $ model
        in childKeys `Map.union` ((Map.map . second . result) applyToModel $
                                  keysMap size model mutable items)
    }

type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable ^> aMutableCursor

delegatedMutable :: Bool -> Cursor -> DelegatedMutable
delegatedMutable startInside cursor =
    (FocusDelegator.Mutable startInside, Mutable cursor)

newDelegatedWith :: Color -> Cursor -> Items model -> Widget.New model DelegatedMutable
newDelegatedWith focusColor size items acc =
    let grid = new size items $ acc ^> FocusDelegator.aDelegatedMutable
    in FocusDelegator.newWith focusColor "Go in" "Go out" grid $
           acc ^> FocusDelegator.aFocusDelegatorMutable

newDelegated :: Cursor -> Items model -> Widget.New model DelegatedMutable
newDelegated = newDelegatedWith FocusDelegator.defaultFocusColor
