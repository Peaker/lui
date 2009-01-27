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

type NewGrid = Cursor -> Cursor -> Map.Map Cursor Item -> Widget.AnyWidgetState

new :: NewGrid
new size cursor items = Widget.AnyWidgetState (Grid size) $ State cursor items

newDelegated :: SDL.Color -> Bool -> NewGrid
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

moveX, moveY :: (Int -> Int) -> Grid -> State -> State
moveX delta (Grid (sizex, _)) (State oldCursor items) = State newCursor items
    where 
      newCursor = first (max 0 . min (sizex-1) . delta) oldCursor
moveY delta (Grid (_, sizey)) (State oldCursor items) = State newCursor items
    where 
      newCursor = second (max 0 . min (sizey-1) . delta) oldCursor

actMoveLeft, actMoveRight, actMoveUp, actMoveDown :: (String, Grid -> State -> State)
actMoveLeft  = ("Move left",  moveX (subtract 1))
actMoveRight = ("Move right", moveX (+1))
actMoveUp    = ("Move up",    moveY (subtract 1))
actMoveDown  = ("Move down",  moveY (+1))

keysMap :: Grid -> State -> Widget.ActionHandlers State
keysMap grid state = Map.fromList $
    let (x,y) = stateCursor state
        (sx,sy) = gridSize grid
    in map (((,) Widget.KeyDown . asKeyGroup noMods) ***
            second (const . ($state) . ($grid))) $ concat $
           [
            cond (x > 0)    (SDL.SDLK_LEFT,  actMoveLeft)
           ,cond (x < sx-1) (SDL.SDLK_RIGHT, actMoveRight)
           ,cond (y > 0)    (SDL.SDLK_UP,    actMoveUp)
           ,cond (y < sy-1) (SDL.SDLK_DOWN,  actMoveDown)
           ]
    where
      cond p i = if p then [i] else []

inFrac :: (Integral a, RealFrac b) => (b -> b) -> a -> a
inFrac = fromIntegral ~> floor

instance Widget.Widget Grid State where
    getKeymap grid state@(State cursor items) =
        case selectedItem grid state of
          Nothing -> keysMap grid state
          Just (Item alignments (Widget.AnyWidgetState child oldChildState)) ->
              (Map.map . second . result)
              (\newChildState -> State cursor $
                                 Map.insert cursor (Item alignments $ Widget.AnyWidgetState child newChildState) items)
              (Widget.getKeymap child oldChildState)
              `Map.union`
              keysMap grid state


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
