{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.Grid where

import qualified Widget
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified MySDLKey
import MySDLKey(asKeyGroup, noMods)
import qualified MySDLKeys
import qualified Draw
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Vector2(Vector2(..)
              , vector2fst, vector2snd)
import Data.Map((!))
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
    }

data State = State
    {
      stateCursor :: Cursor
    , stateSize :: Cursor
    , stateItems :: Map.Map Cursor Item
    }

type NewGrid = Cursor -> Cursor -> Map.Map Cursor Item -> Widget.AnyWidgetState

new :: NewGrid
new cursor size items = Widget.AnyWidgetState Grid $ State cursor size items

newDelegated :: SDL.Color -> Bool -> NewGrid
newDelegated gridFocusColor startInItem cursor size items =
    FocusDelegator.new "Go in" "Go out" gridFocusColor startInItem $
                       new cursor size items

selectedItem :: Grid -> State -> Item
selectedItem _ (State cursor _ items) = items ! cursor

selectedWidget :: Grid -> State -> Widget.AnyWidgetState
selectedWidget grid state = itemWidgetState $ selectedItem grid state

gridRows :: Grid -> State -> [[(Cursor, Item)]]
gridRows _ (State _ (sizex, sizey) items) =
    [[((x,y), items ! (x,y)) | x <- [0..sizex-1]] | y <- [0..sizey-1]]

gridDrawInfo :: Grid -> State -> Cursor -> Widget.DrawInfo
gridDrawInfo _ (State cursor _ _) itemIndex =
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

moveX, moveY :: (Int -> Int) -> State -> State
moveX delta (State oldCursor size@(sizex, _) items) = State newCursor size items
    where 
      newCursor = first (max 0 . min (sizex-1) . delta) oldCursor
moveY delta (State oldCursor size@(_, sizey) items) = State newCursor size items
    where 
      newCursor = second (max 0 . min (sizey-1) . delta) oldCursor

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
    getKeymap grid state@(State cursor size items) =
        case selectedItem grid state of
          Item alignments (Widget.AnyWidgetState child oldChildState) ->
              (Map.map . second . result)
              (\newChildState -> State cursor size $
                                 Map.insert cursor (Item alignments $ Widget.AnyWidgetState child newChildState) items)
              (Widget.getKeymap child oldChildState)
              `Map.union`
              keysMap state


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
            \((xpos, width), (itemIndex,item)) -> do
              let Item (ax, ay) widget = item
                  drawInfo = gridDrawInfo grid state itemIndex
                  onWidget = (widget `Widget.onAnyWidgetState`)
              Vector2 w h <- Draw.computeToDraw $ onWidget (Widget.size drawInfo)
              let pos = Vector2 (xpos + inFrac (*ax) (width-w))
                                (ypos + inFrac (*ay) (height-h))
              Draw.move pos $ onWidget (Widget.draw drawInfo)
      return $ Vector2 (sum columnWidths) (sum rowHeights)
