{-# OPTIONS_GHC -Wall -O2
 #-}

module Example(makeGui, guiModel) where

import Graphics.UI.LUI.Widget(Widget)
import qualified Graphics.UI.LUI.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.LUI.Widgets.TextView as TextView
import qualified Graphics.UI.LUI.Widgets.Grid as Grid
import qualified Graphics.UI.LUI.Widgets.Box as Box
import qualified Graphics.UI.LUI.Widgets.Space as Space
import qualified Graphics.UI.LUI.Widgets.KeysTable as KeysTable
import Graphics.UI.HaskGame.Font(Font)
import qualified Graphics.UI.HaskGame.Font as Font
import Graphics.UI.LUI.Accessor(Accessor, accessor, aMapValue, (^>), (^.))
import Graphics.UI.HaskGame.Color(Color(..))
import qualified Data.Map as Map
import Data.Maybe(listToMaybe)
import Graphics.UI.LUI.List(isSorted)
import Control.Monad(mapM)

-- Model:
data Model = Model
    {
      vboxModel :: Box.DelegatedMutable
    , hboxModel :: Box.Mutable
    , textEditModels :: Map.Map Grid.Cursor TextEdit.DelegatedMutable
    , gridModel :: Grid.DelegatedMutable
    }

data Fonts = Fonts
    {
      defaultFont, textViewFont, keysFont, descFont :: Font
    }

-- TODO: Replace with TH auto-gen
avboxModel :: Accessor Model Box.DelegatedMutable
avboxModel = accessor vboxModel (\new x -> x{vboxModel=new})
ahboxModel :: Accessor Model Box.Mutable
ahboxModel = accessor hboxModel (\new x -> x{hboxModel=new})
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
    , hboxModel = Box.Mutable 0
    , textEditModels =
      Map.fromList [((x, y),
                     TextEdit.delegatedMutable False (texts!!(y*2+x)) 5)
                    | x <- [0..1]
                   , y <- [0..1]]
    , gridModel = Grid.delegatedMutable False (0, 0)
    }


-- Widgets

textEditCursorColor, textViewColor, textEditColor, textEditingColor :: Color
textEditingColor = Color 30 20 100
textEditColor = Color 255 255 255
textViewColor = Color 255 100 255
textEditCursorColor = Color 255 0 0

textEdit :: Grid.Cursor -> Fonts -> Widget Model
textEdit cursor fonts =
    TextEdit.newDelegated textEditingColor
                          textEditCursorColor
                          (defaultFont fonts)
                          textEditColor $
                          atextEditModels ^> aMapValue cursor
                          

textView :: String -> Fonts -> Widget Model
textView text fonts =
    TextView.new textViewColor (textViewFont fonts) text

grid, hbox, vbox, keysTable, proxy1, proxy2 :: Fonts -> Widget Model

gridSize :: Grid.Cursor
gridSize = (2, 2)

grid fonts =
    Grid.newDelegated gridSize items agridModel
    where
      items = Map.fromList
              [((x, y), Grid.Item (textEdit (x, y) fonts) (0.5, 1))
               | x <- [0..1], y <- [0..1]]

hbox fonts =
    Box.new Box.Horizontal items ahboxModel
    where
      items = [Box.Item (vbox fonts) 0.5
              ,Box.Item (Space.newW 50) 0
              ,Box.Item (keysTable fonts) 0]

vbox fonts = Box.newDelegated Box.Vertical items avboxModel
    where
      items = [Box.Item (grid fonts) 1
              ,Box.Item (Space.newH 100) 0.5
              ,Box.Item (proxy1 fonts) 0.5
              ,Box.Item (textView "This is just a view" fonts) 0.5
              ,Box.Item (proxy2 fonts) 0.5]

keysTable fonts = KeysTable.newForWidget (keysFont fonts) (descFont fonts) (hbox fonts)

proxy1 fonts model =
    textEdit (model ^. agridModel ^. Grid.aDelegatedMutableCursor) fonts model

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

proxy2 fonts model =
    let cursor = model ^. agridModel ^. Grid.aDelegatedMutableCursor
        text = model ^. atextEditModels ^. aMapValue cursor ^.
               TextEdit.aDelegatedMutableText
    in maybe (textView ("Invalid cursor position selected: " ++ text) fonts model)
             (\cur -> textEdit cur fonts model) $
       readCursor text

makeGui :: IO (Widget Model)
makeGui = do
  [f15, f25, f30] <- mapM Font.defaultFont [15, 25, 30]
  return . hbox $ Fonts f30 f15 f25 f25
