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
      itemAlignments :: (Double, Double)

    , itemWidgetState :: Widget.AnyWidgetState
    }

data Grid = Grid
    {
      gridCursorBGColor :: SDL.Color
    }

type Cursor = (Int, Int)

data State = State
    {
      stateCursor :: Cursor
    , stateItems :: Array.Array Cursor Item
    }

new :: SDL.Color -> Cursor -> Array.Array Cursor Item -> Widget.AnyWidgetState
new cursorBGColor cursor items = Widget.AnyWidgetState (Grid cursorBGColor) $
                                 State cursor items

selectedItem :: Grid -> State -> Item
selectedItem _ (State cursor items) = items ! cursor

selectedWidget :: Grid -> State -> Widget.AnyWidgetState
selectedWidget grid state = itemWidgetState $ selectedItem grid state

gridRows :: Grid -> State -> [[(Cursor, Item)]]
gridRows _ (State _ items) =
    let ((startx, starty), (endx, endy)) = Array.bounds items
    in [[((x,y), items ! (x,y)) | x <- [startx..endx]] | y <- [starty..endy]]

gridDrawInfo :: Grid -> State -> Cursor -> Widget.DrawInfo
gridDrawInfo _ (State cursor _) itemIndex =
    Widget.DrawInfo (cursor==itemIndex)

rowColumnSizes :: Grid -> State -> Draw.Compute ([Int], [Int])
rowColumnSizes grid state = do
  rowsSizes <- forM (gridRows grid state) . mapM $ \(itemIndex, item) ->
                   Widget.onAnyWidgetState (itemWidgetState item) $
                   Widget.size $ gridDrawInfo grid state itemIndex

  let rowsHeights = (map . map) vector2snd rowsSizes
      rowsWidths = (map . map) vector2fst rowsSizes
      rowHeights = map maximum $ rowsHeights
      columnWidths = map maximum . transpose $ rowsWidths
  return (rowHeights, columnWidths)

putInBounds :: Ord a => (a, a) -> a -> a
putInBounds (low, high) = max low . min high

xBounds, yBounds :: Array.Array Cursor Item -> Cursor
xBounds items = (minX, maxX)
    where
      ((minX, _), (maxX, _)) = Array.bounds items
yBounds items = (minY, maxY)
    where
      ((_, minY), (_, maxY)) = Array.bounds items
putInXBounds, putInYBounds :: Array.Array Cursor Item -> Int -> Int
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

    size _ grid state = do
      (rowHeights, columnWidths) <- rowColumnSizes grid state
      return $ Vector2.Vector2 (sum columnWidths) (sum rowHeights)

    draw _ grid state = do
      (rowHeights, columnWidths) <- Draw.computeToDraw $ rowColumnSizes grid state
      let posSizes sizes = let positions = scanl (+) 0 sizes
                           in zip positions sizes
          rows = gridRows grid state
      forM_ (zip (posSizes rowHeights) rows) $
        \((ypos, height), row) -> do
          forM_ (zip (posSizes columnWidths) row) $
            \((xpos, width), (itemIndex@(x,y),item)) -> do
              let Item (ax, ay) widget = item
                  drawInfo = gridDrawInfo grid state itemIndex
                  onWidget = (widget `Widget.onAnyWidgetState`)
              widgetSize@(Vector2 w h) <- Draw.computeToDraw $ onWidget (Widget.size drawInfo)
              let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                (ypos + inFrac (*ay) (height-h))
              Draw.move pos $ do
                if stateCursor state == (x, y)
                  then do
                    Draw.rect (gridCursorBGColor grid) widgetSize
                    return ()
                  else
                    return ()
                onWidget (Widget.draw drawInfo)
      return $ Vector2 (sum columnWidths) (sum rowHeights)
