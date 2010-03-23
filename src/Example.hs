{-# OPTIONS_GHC -Wall -O2 #-}

module Main(main) where

import qualified Graphics.UI.LUI.Run as Run
import qualified Graphics.UI.LUI.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.LUI.Widgets.TextView as TextView
--import qualified Graphics.UI.LUI.Widgets.Scroll as Scroll
import qualified Graphics.UI.LUI.Widgets.Grid as Grid
import qualified Graphics.UI.LUI.Widgets.Box as Box
import qualified Graphics.UI.LUI.Widgets.Space as Space
--import qualified Graphics.UI.LUI.Widgets.KeysTable as KeysTable
--import qualified Graphics.UI.LUI.Widgets.Adapter as Adapter
import Graphics.UI.LUI.Widget(Widget)
import Data.Accessor(Accessor, accessor, (^.))
import Data.Accessor.Map(aMapValue)

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Color(..), Font)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($~), ($=))

import qualified Data.Map as Map
import Data.Maybe(fromMaybe, listToMaybe)
import Control.Category((>>>))
import Control.Concurrent.MVar(newMVar, readMVar, modifyMVar_)

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

-- Model:
data Model = Model
    {
      vboxModel :: Box.DelegatedMutable
    , textEditModels :: Map.Map Grid.Cursor TextEdit.DelegatedMutable
    , gridModel :: Grid.DelegatedMutable
--    , scrollModel :: Scroll.Mutable
    }

data Fonts = Fonts
    {
      defaultFont, textViewFont, keysFont, descFont :: Draw.Font
    }

-- TODO: Replace with TH auto-gen
avboxModel :: Accessor Model Box.DelegatedMutable
avboxModel = accessor vboxModel (\new x -> x{vboxModel=new})
-- scrollerModel :: Accessor Model Scroll.Mutable
-- scrollerModel = accessor scrollModel (\new x -> x{scrollModel=new})
atextEditModels :: Accessor Model (Map.Map Grid.Cursor TextEdit.DelegatedMutable)
atextEditModels = accessor textEditModels (\new x -> x{textEditModels=new})
agridModel :: Accessor Model Grid.DelegatedMutable
agridModel = accessor gridModel (\new x -> x{gridModel=new})

texts :: [String]
texts =
    [
     "Hello"
    ,"World"
    ,"Blah"
    ,"Bleh"
    ]

guiModel :: Model
guiModel =
    Model
    {
      vboxModel = Box.delegatedMutable False 0
    , textEditModels =
      Map.fromList [((x, y),
                     TextEdit.delegatedMutable False text (length text))
                    | x <- [0..1]
                    , y <- [0..1]
                    , let text = texts !! (y*2 + x)]
    , gridModel = Grid.delegatedMutable False (0, 0)
    -- , scrollModel = Scroll.Mutable $ Vector2 0 0
    }


-- Widgets

textEditCursorColor, textViewColor, textEditColor, textEditingColor :: Color
textEditingColor = Color 0.15 0.20 0.3 1
textEditColor = Color 1 1 1 1
textViewColor = Color 1 0.4 1 1
textEditCursorColor = Color 1 0 0 1

textEdit :: Grid.Cursor -> Fonts -> Widget Model
textEdit cursor fonts =
    TextEdit.newDelegated textEditingColor
                          textEditCursorColor
                          (defaultFont fonts)
                          textEditColor $
                          atextEditModels >>> aMapValue cursor

gridSize :: Grid.Cursor
gridSize = (2, 2)

grid :: Fonts -> Widget Model
grid fonts =
    Grid.newDelegated gridSize items agridModel
    where
      items = Map.fromList
              [((x, y), Grid.Item (textEdit (x, y) fonts) (0.5, 1))
               | x <- [0..1], y <- [0..1]]

proxy1 :: Fonts -> Widget Model
proxy1 fonts model =
    textEdit (model ^. (agridModel >>> Grid.aDelegatedMutableCursor)) fonts model

simpleRead :: Read a => String -> Maybe a
simpleRead = listToMaybe . map fst . filter (null . snd) . reads

readCursor :: String -> Maybe Grid.Cursor
readCursor text =
    let (xCount, yCount) = gridSize
        verifyCursor cursor@(x, y) =
            if isSorted [0, x, xCount-1] &&
               isSorted [0, y, yCount-1]
            then Just cursor
            else Nothing
    in verifyCursor =<< simpleRead text

textView :: String -> Font -> Widget Model
textView text font =
    TextView.new textViewColor font text

proxy2 :: Fonts -> Widget Model
proxy2 fonts model =
  maybe invalidPos (\cur -> textEdit cur fonts model) $ readCursor text
  where
    invalidPos = textView ("Invalid cursor position selected: " ++ text)
                 (textViewFont fonts) model
    cursor = model ^. (agridModel >>> Grid.aDelegatedMutableCursor)
    text = model ^. (atextEditModels >>> aMapValue cursor >>>
                     TextEdit.aDelegatedMutableText)

-- scrollBox :: Fonts -> Widget Model
-- scrollBox fonts = Scroll.new (Vector2 200 200) box scrollerModel
--     where
--       font = defaultFont fonts
--       box = Box.new Box.Vertical items $ Box.noAcc 0
--       items = [Box.Item (Adapter.adaptImage
--                          (Image.cropRect $ Rect i 0 (w-i-i) h) $
--                          textView text font) 0.5
--                | i <- [0,20..250]
--               , let text = "THIS IS A TRUNCATED VIEW: " ++ show i
--                     Vector2 w h = Image.textSize font text]

vbox :: Fonts -> Widget Model
vbox fonts = Box.newDelegated Box.Vertical items avboxModel
    where
      items = [Box.Item (grid fonts) 1
              ,Box.Item (Space.newH 0.1) 0.5
              ,Box.Item (proxy1 fonts) 0.5
              ,Box.Item (proxy2 fonts) 0.5
--              ,Box.Item (scrollBox fonts) 0.5
              ]

withKeysTable :: Fonts -> Widget Model
withKeysTable fonts = --KeysTable.newBoxedWidget Box.Horizontal 50 (keysFont fonts) (descFont fonts)
                      (vbox fonts)

fontFilename :: FilePath
fontFilename = "/usr/share/fonts/truetype/freefont/FreeMono.ttf"

makeGui :: IO (Widget Model)
makeGui = do
  font <- Draw.openFont fontFilename
  return . withKeysTable $ Fonts font font font font

main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialWindowSize $= GLUT.Size 800 600
  GLUT.initialDisplayMode $~ (GLUT.DoubleBuffered:)
  GLUT.createWindow "example"

  gui <- makeGui
  curModel <- newMVar guiModel
  GLUT.idleCallback $=
    Just ((Run.draw . gui =<< readMVar curModel) >>
          GLUT.swapBuffers)
  GLUT.keyboardMouseCallback $=
    Just (\key keyState mods mousePos ->
           case keyState of
             GLUT.Up -> return ()
             GLUT.Down ->
               modifyMVar_ curModel
               (\model -> return . fromMaybe model .
                          Run.keyEvent (mods, key) . gui $ model))
  GLUT.mainLoop
