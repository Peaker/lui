{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Grid where

import qualified Widget
import Widget(Widget(..))

-- import qualified Widgets.FocusDelegator as FocusDelegator
import qualified MySDLKey
import MySDLKey(asKeyGroup, noMods)
import qualified Draw
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Vector2(Vector2(..)
              , vector2fst, vector2snd)
import Data.List(transpose)
import Control.Monad(forM_, forM)
import Control.Arrow(first, second, (***))
import Func((~>), result)
import Data.Maybe(isJust, fromMaybe)
import List(isSorted)
import Tuple(swap)
import Accessor((^.), (^:))

data Item model = Item
    {
      -- alignments are a number between 0..1 that
      -- represents where in the grid item's left..right
      -- and top..down the item should be in
      itemAlignments :: (Double, Double)

    , itemWidget :: Widget model
    }

type Items model = Map.Map Cursor (Item model)
type Cursor = (Int, Int)

data Mutable = Mutable
    {
      mutableCursor :: Cursor
    }

selectedItem :: Mutable -> Items model -> Maybe (Item model)
selectedItem (Mutable cursor) items = cursor `Map.lookup` items

gridRows :: Cursor -> Items model -> [[(Cursor, Maybe (Item model))]]
gridRows (sizex, sizey) items =
    [[((x,y), (x,y) `Map.lookup` items) | x <- [0..sizex-1]] | y <- [0..sizey-1]]

gridDrawInfo :: Mutable -> Cursor -> Widget.DrawInfo -> Widget.DrawInfo
gridDrawInfo (Mutable cursor) itemIndex (Widget.DrawInfo drawInfo) =
    Widget.DrawInfo (drawInfo && cursor==itemIndex)

rowColumnSizes :: model -> Mutable -> Items model -> Cursor -> Widget.DrawInfo ->
                  Draw.Compute ([Int], [Int])
rowColumnSizes model mutable items size drawInfo = do
  rowsSizes <- forM (gridRows size items) . mapM $ \(itemIndex, mItem) ->
                   case mItem of
                     Nothing -> return $ Vector2 0 0
                     Just item ->
                         widgetSize (itemWidget item)
                                    (gridDrawInfo mutable itemIndex drawInfo)
                                    model

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
itemSelectable model (Item _ widget) =
    isJust $ widgetGetKeymap widget model

getSelectables :: ((Int, Int) -> (Int, Int)) ->
                  Cursor -> model -> Mutable -> Items model -> (Int -> Int) ->
                  [(Int, Int)]
getSelectables toSwap size model (Mutable oldCursor) items cursorFunc =
    let (sizeA,_) = toSwap size
        (oldA,b) = toSwap oldCursor
        nexts = takeWhile (\a -> isSorted [0, a, sizeA-1]) .
                drop 1 . iterate cursorFunc $ oldA
        coor a = toSwap (a, b)
        itemAt a = (coor a, items Map.! coor a)
    in map fst . filter (itemSelectable model . snd) . map itemAt $ nexts

getSelectablesX, getSelectablesY :: Cursor -> model -> Mutable -> Items model ->
                                    (Int -> Int) -> [(Int, Int)]
getSelectablesX = getSelectables id
getSelectablesY = getSelectables swap

keysMap :: Cursor -> model -> Mutable -> Items model -> Widget.ActionHandlers Mutable
keysMap size model mutable items =
    Map.fromList $
       map (((,) Widget.KeyDown . asKeyGroup noMods) ***
            second const) . concat $
           [let opts = axis size model mutable items direction
            in cond opts (key, (desc, head opts `mutableMoveTo` mutable))
            | (key, axis, direction, desc) <-
                [(SDL.SDLK_LEFT,  getSelectablesX, (subtract 1), "Move left")
                ,(SDL.SDLK_RIGHT, getSelectablesX, (+1), "Move right")
                ,(SDL.SDLK_UP,    getSelectablesY, (subtract 1), "Move up")
                ,(SDL.SDLK_DOWN,  getSelectablesY, (+1), "Move down")
                ]
           ]
    where
      cond p i = if not . null $ p then [i] else []

inFrac :: (Integral a, RealFrac b) => (b -> b) -> a -> a
inFrac = fromIntegral ~> floor

type New model mutable =
    Cursor -> Items model -> Widget.New model mutable

posSizes :: [Int] -> [(Int, Int)]
posSizes sizes =
    let positions = scanl (+) 0 sizes
    in zip positions sizes

new :: New model Mutable
new size items accessor =
    Widget
    {
      widgetDraw = \drawInfo model -> do
        let mutable = model ^. accessor
            rows = gridRows size items
        (rowHeights, columnWidths) <- Draw.computeToDraw $
                                      rowColumnSizes model mutable items size drawInfo
        forM_ (zip (posSizes rowHeights) rows) $
          \((ypos, height), row) -> do
            forM_ (zip (posSizes columnWidths) row) $
              \((xpos, width), (itemIndex, mItem)) ->
                case mItem of
                  Nothing -> return ()
                  Just item -> do
                    let Item (ax, ay) childWidget = item
                        childDrawInfo = gridDrawInfo mutable itemIndex drawInfo
                    Vector2 w h <- Draw.computeToDraw $
                                   widgetSize childWidget childDrawInfo model
                    let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                      (ypos + inFrac (*ay) (height-h))
                    Draw.move pos $ widgetDraw childWidget childDrawInfo model
                    return ()
        return $ Vector2 (sum columnWidths) (sum rowHeights)

    , widgetSize = \drawInfo model -> do
      let mutable = model ^. accessor
      (rowHeights, columnWidths) <- rowColumnSizes model mutable items size drawInfo
      return $ Vector2 (sum columnWidths) (sum rowHeights)

    , widgetGetKeymap = \model -> Just $
      let mutable = model ^. accessor
          childKeys = fromMaybe Map.empty $ do
            Item _ childWidget <- selectedItem mutable items
            widgetGetKeymap childWidget model
          applyToModel newMutable = (accessor ^: const newMutable) model
      in childKeys `Map.union`
         ((Map.map . second . result) applyToModel $
          keysMap size model mutable items)
    }

-- newDelegated :: SDL.Color -> Bool ->
--                 New (FocusDelegator.FocusDelegatorMutable Immutable Mutable)
-- newDelegated gridFocusColor startInItem size cursor items =
--     FocusDelegator.new "Go in" "Go out" gridFocusColor startInItem $
--                        new size cursor items
