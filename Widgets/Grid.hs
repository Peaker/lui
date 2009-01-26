{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Grid where

import qualified Widget
import qualified MySDLKey
import MySDLKey(asKeyGroup, noMods)
import qualified MySDLKeys
import qualified Draw
import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Vector2(Vector2(..)
              , vector2fst, vector2snd)
import Data.Array((!))
import Data.List(transpose)
import Control.Monad(forM_, forM)
import Control.Arrow(first, second, (***))
import Func((~>), result)

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

putInBounds :: Ord a => (a, a) -> a -> a
putInBounds (low, high) = max low . min high

xBounds, yBounds :: Array.Array (Int,Int) Item -> (Int, Int)
xBounds items = (minX, maxX)
    where
      ((minX, _), (maxX, _)) = Array.bounds items
yBounds items = (minY, maxY)
    where
      ((_, minY), (_, maxY)) = Array.bounds items
putInXBounds, putInYBounds :: Array.Array (Int,Int) Item -> Int -> Int
putInXBounds = putInBounds . xBounds
putInYBounds = putInBounds . yBounds

moveX, moveY :: (Int -> Int) -> State -> State
moveX delta (State oldCursor items) = State newCursor items
    where 
      newCursor = first (putInXBounds items . delta) oldCursor
moveY delta (State oldCursor items) = State newCursor items
    where 
      newCursor = second (putInYBounds items . delta) oldCursor

actMoveLeft, actMoveRight, actMoveUp, actMoveDown :: (String, State -> State)
actMoveLeft  = ("Move left",  moveX (subtract 1))
actMoveRight = ("Move right", moveX (+1))
actMoveUp    = ("Move up",    moveY (subtract 1))
actMoveDown  = ("Move down",  moveY (+1))

keysMap :: State -> Widget.ActionHandlers State
keysMap state =
    Map.fromList . map (((,) Widget.KeyDown) *** second ($state)) $ actions

actions :: [(MySDLKeys.KeyGroup,
             (String, State -> MySDLKey.Key -> State))]
actions =
    map (asKeyGroup noMods *** ignoreKey)
    [(SDL.SDLK_LEFT, actMoveLeft)
    ,(SDL.SDLK_RIGHT, actMoveRight)
    ,(SDL.SDLK_UP, actMoveUp)
    ,(SDL.SDLK_DOWN, actMoveDown)]
    -- TODO: ignoreKey should be shared somehow?
    where
      -- ignoreKey adds an ignored (MySDLKey.Key ->) to the
      -- result State in the handlers
      ignoreKey = (second . result) const

inFrac :: (Integral a, RealFrac b) => (b -> b) -> a -> a
inFrac = fromIntegral ~> floor

instance Widget.Widget Grid State where
    getKeymap _ = keysMap

    size grid state = do
      (rowHeights, columnWidths) <- rowColumnSizes grid state
      return $ Vector2.Vector2 (sum columnWidths) (sum rowHeights)

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
