{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.Grid where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

import qualified Widgets.FocusDelegator as FocusDelegator
import qualified HaskGame.Key as Key
import HaskGame.Key(asKeyGroup, noMods, shift)
import HaskGame.Color(Color)
import HaskGame.Vector2(Vector2(..)
                       ,vector2fst,vector2snd)
import qualified Draw
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Data.List(transpose)
import Control.Monad(forM_, forM)
import Control.Arrow(first, second, (***))
import Func((~>), result)
import Data.Maybe(isJust, fromMaybe)
import List(isSorted)
import Tuple(swap)
import Accessor(Accessor, convertor, (^.), (^>), write)
import Data.Maybe(isNothing)

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
                     Draw.Compute ([Int], [Int])
getRowColumnSizes model mutable items size drawInfo = do
  rowsSizes <- forM (gridRows size items) . mapM $ \(itemIndex, mItem) ->
                   case mItem of
                     Nothing -> return $ Vector2 0 0
                     Just item ->
                         widgetSize ((itemWidget item) model)
                                    (gridDrawInfo mutable itemIndex drawInfo)

  let rowsHeights = (map . map) vector2snd rowsSizes
      rowsWidths = (map . map) vector2fst rowsSizes
      rowHeights = map maximum $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths
  return (rowHeights, columnWidths)

mutableCursorApply :: (Cursor -> Cursor) -> Mutable -> Mutable
mutableCursorApply func (Mutable oldCursor) = Mutable $ func oldCursor

mutableMoveTo :: (Int, Int) -> Mutable -> Mutable
mutableMoveTo newCursor = mutableCursorApply (const newCursor)

moveX, moveY :: (Int -> Int) -> Cursor -> Cursor -> Cursor
moveX delta (sizex, _) oldCursor =
    first (max 0 . min (sizex-1) . delta) oldCursor
moveY delta (_, sizey) oldCursor =
    second (max 0 . min (sizey-1) . delta) oldCursor

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
      widgetDraw = \drawInfo -> do
        (rowHeights, columnWidths) <- Draw.computeToDraw . rowColumnSizes $ drawInfo
        forM_ (zip (posSizes rowHeights) rows) $
          \((ypos, height), row) -> do
            forM_ (zip (posSizes columnWidths) row) $
              \((xpos, width), (itemIndex, mItem)) ->
                case mItem of
                  Nothing -> return ()
                  Just item -> do
                    let Item childWidget (ax, ay) = item
                        childDrawInfo = gridDrawInfo mutable itemIndex drawInfo
                        childWidgetFuncs = childWidget model
                    Vector2 w h <- Draw.computeToDraw $
                                   widgetSize childWidgetFuncs childDrawInfo
                    let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                      (ypos + inFrac (*ay) (height-h))
                    Draw.move pos $ widgetDraw childWidgetFuncs childDrawInfo
                    return ()
        return $ Vector2 (sum columnWidths) (sum rowHeights)

    , widgetSize = \drawInfo -> do
      (rowHeights, columnWidths) <- rowColumnSizes drawInfo
      return $ Vector2 (sum columnWidths) (sum rowHeights)

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
