{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Grid where

import qualified Widget
import qualified Draw
import qualified Data.Array as Array
import qualified Graphics.UI.SDL as SDL
import Vector2(Vector2(..)
              , vector2fst, vector2snd)
import Data.Array((!))
import Control.Monad(forM_, forM)
import Data.List(transpose)
import Func((~>))

data Item = Item
    {
      -- alignments are a number between 0..1 that
      -- represents where in the grid item's left..right
      -- and top..down the item should be in
      itemAlignments :: (Double, Double),

      itemWidgetState :: Widget.AnyWidgetState
    }

data Grid = Grid
    {
      gridCursorBGColor :: SDL.Color
    }

data State = State
    {
      stateCursor :: (Int, Int),
      stateItems :: Array.Array (Int,Int) Item
    }

new :: SDL.Color -> (Int, Int) -> Array.Array (Int,Int) Item -> Widget.AnyWidgetState
new cursorBGColor cursor items = Widget.AnyWidgetState (Grid cursorBGColor) $
                                 State cursor items

selectedItem :: Grid -> State -> Item
selectedItem _ (State cursor items) = items ! cursor

selectedWidget :: Grid -> State -> Widget.AnyWidgetState
selectedWidget grid state = itemWidgetState $ selectedItem grid state

gridRows :: Grid -> State -> [[Item]]
gridRows _ (State _ items) =
    let ((startx, starty), (endx, endy)) = Array.bounds items
    in [[items ! (x,y) | x <- [startx..endx]] | y <- [starty..endy]]

rowColumnSizes :: Grid -> State -> Draw.Compute ([Int], [Int])
rowColumnSizes grid state = do
  rowsSizes <- forM (gridRows grid state) . mapM $
               (Widget.size `Widget.onAnyWidgetState`) . itemWidgetState
  let rowsHeights = (map . map) vector2snd rowsSizes
      rowsWidths = (map . map) vector2fst rowsSizes
      rowHeights = map maximum $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths
  return (rowHeights, columnWidths)


-- goRight :: State -> State
-- goRight (State cursor _) = State text (length text)

inFrac :: (Integral a, RealFrac b) => (b -> b) -> a -> a
inFrac = fromIntegral ~> floor

instance Widget.Widget Grid State where
    draw grid state = do
      (rowHeights, columnWidths) <- Draw.computeToDraw $ rowColumnSizes grid state
      let rowPositions = scanl (+) 0 rowHeights
          columnPositions = scanl (+) 0 columnWidths
          rows = gridRows grid state
      forM_ (zip [0..] (zip3 rowPositions rowHeights rows)) $
        \(y, (ypos, height, row)) -> do
          forM_ (zip [0..] (zip3 columnPositions columnWidths row)) $
            \(x, (xpos, width, item)) -> do
              let Item (ax, ay) widget = item
                  onWidget = (`Widget.onAnyWidgetState` widget)
              widgetSize@(Vector2 w h) <- Draw.computeToDraw $ onWidget Widget.size
              let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                (ypos + inFrac (*ay) (height-h))
              Draw.move pos $ do
                if stateCursor state == (x, y)
                  then do
                    Draw.rect (gridCursorBGColor grid) widgetSize
                    return ()
                  else
                    return ()
                onWidget Widget.draw
      return $ Vector2 (last columnPositions) (last rowPositions)
    size grid state = do
      (rowHeights, columnWidths) <- rowColumnSizes grid state
      return $ Vector2.Vector2 (sum columnWidths) (sum rowHeights)
    getKeymap = undefined
