{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Grid where

import qualified Widget
import qualified Widgets.FocusDelegator as FocusDelegator
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

data Item = Item
    {
      -- alignments are a number between 0..1 that
      -- represents where in the grid item's left..right
      -- and top..down the item should be in
      itemAlignments :: (Double, Double)

    , itemWidgetState :: Widget.AnyWidgetState
    }

type Cursor = (Int, Int)

data Grid = Grid
    {
      gridSize :: Cursor
    }

data State = State
    {
      stateCursor :: Cursor
    , stateItems :: Map.Map Cursor Item
    }

type GridState = Widget.WidgetState Grid State
type New w = Cursor -> Cursor -> Map.Map Cursor Item -> w

new :: New GridState
new size cursor items = Widget.WidgetState (Grid size) $ State cursor items

newDelegated :: SDL.Color -> Bool ->
                New (FocusDelegator.FocusDelegatorState Grid State)
newDelegated gridFocusColor startInItem size cursor items =
    FocusDelegator.new "Go in" "Go out" gridFocusColor startInItem $
                       new size cursor items

selectedItem :: Grid -> State -> Maybe Item
selectedItem _ (State cursor items) = cursor `Map.lookup` items

gridRows :: Grid -> State -> [[(Cursor, Maybe Item)]]
gridRows (Grid (sizex, sizey)) (State _ items) =
    [[((x,y), (x,y) `Map.lookup` items) | x <- [0..sizex-1]] | y <- [0..sizey-1]]

gridDrawInfo :: Grid -> State -> Cursor -> Widget.DrawInfo -> Widget.DrawInfo
gridDrawInfo _ (State cursor _) itemIndex (Widget.DrawInfo drawInfo) =
    Widget.DrawInfo (drawInfo && cursor==itemIndex)

rowColumnSizes :: Grid -> State -> Widget.DrawInfo -> Draw.Compute ([Int], [Int])
rowColumnSizes grid state drawInfo = do
  rowsSizes <- forM (gridRows grid state) . mapM $ \(itemIndex, mItem) ->
                   case mItem of
                     Nothing -> return $ Vector2 0 0
                     Just item ->
                         Widget.onAnyWidgetState (itemWidgetState item) $
                         Widget.size $ gridDrawInfo grid state itemIndex drawInfo

  let rowsHeights = (map . map) vector2snd rowsSizes
      rowsWidths = (map . map) vector2fst rowsSizes
      rowHeights = map maximum $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths
  return (rowHeights, columnWidths)

stateCursorApply :: (Cursor -> Cursor) -> State -> State
stateCursorApply func (State oldCursor items) = State (func oldCursor) items

stateMoveTo :: (Int, Int) -> State -> State
stateMoveTo newCursor = stateCursorApply (const newCursor)

moveX, moveY :: (Int -> Int) -> Grid -> Cursor -> Cursor
moveX delta (Grid (sizex, _)) oldCursor =
    first (max 0 . min (sizex-1) . delta) oldCursor
moveY delta (Grid (_, sizey)) oldCursor =
    second (max 0 . min (sizey-1) . delta) oldCursor

itemSelectable :: Item -> Bool
itemSelectable (Item _ (Widget.AnyWidgetState widget state)) =
    isJust $ Widget.getKeymap widget state

getSelectables :: ((Int, Int) -> (Int, Int)) ->
                  Grid -> State -> (Int -> Int) ->
                  [(Int, Int)]
getSelectables toSwap (Grid size) (State oldCursor items) cursorFunc =
    let (sizeA,_) = toSwap size
        (oldA,b) = toSwap oldCursor
        nexts = takeWhile (\a -> isSorted [0, a, sizeA-1]) .
                drop 1 . iterate cursorFunc $ oldA
        coor a = toSwap (a, b)
        itemAt a = (coor a, items Map.! coor a)
    in map fst . filter (itemSelectable . snd) . map itemAt $ nexts

getSelectablesX, getSelectablesY :: Grid -> State -> (Int -> Int) ->
                                    [(Int, Int)]
getSelectablesX = getSelectables id
getSelectablesY = getSelectables swap

keysMap :: Grid -> State -> Widget.ActionHandlers State
keysMap grid state =
    Map.fromList $
       map (((,) Widget.KeyDown . asKeyGroup noMods) ***
            second const) . concat $
           [let opts = axis grid state direction
            in cond opts (key, (desc, head opts `stateMoveTo` state))
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

instance Widget.Widget Grid State where
    getKeymap grid state@(State cursor items) = Just $
        let childKeys =
                case selectedItem grid state of
                  Nothing -> Map.empty
                  Just (Item alignments (Widget.AnyWidgetState child oldChildState)) ->
                      fromMaybe Map.empty $
                        (Map.map . second . result)
                        (wrapChildState child alignments)
                        `fmap`
                        (Widget.getKeymap child oldChildState)
        in childKeys `Map.union` keysMap grid state
        where
          wrapChildState child alignments newChildState =
              State cursor $
              Map.insert cursor
                (Item alignments $
                 Widget.AnyWidgetState child newChildState)
              items


    size drawInfo grid state = do
      (rowHeights, columnWidths) <- rowColumnSizes grid state drawInfo
      return $ Vector2 (sum columnWidths) (sum rowHeights)

    draw drawInfo grid state = do
      (rowHeights, columnWidths) <- Draw.computeToDraw $ rowColumnSizes grid state drawInfo
      let posSizes sizes = let positions = scanl (+) 0 sizes
                           in zip positions sizes
          rows = gridRows grid state
      forM_ (zip (posSizes rowHeights) rows) $
        \((ypos, height), row) -> do
          forM_ (zip (posSizes columnWidths) row) $
            \((xpos, width), (itemIndex, mItem)) ->
              case mItem of
                Nothing -> return ()
                Just item -> do
                  let Item (ax, ay) widget = item
                      childDrawInfo = gridDrawInfo grid state itemIndex drawInfo
                      onWidget = (widget `Widget.onAnyWidgetState`)
                  Vector2 w h <- Draw.computeToDraw . onWidget $
                                 Widget.size childDrawInfo
                  let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                    (ypos + inFrac (*ay) (height-h))
                  Draw.move pos . onWidget $ Widget.draw childDrawInfo
                  return ()
      return $ Vector2 (sum columnWidths) (sum rowHeights)
