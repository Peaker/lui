{-# OPTIONS_GHC -Wall -O2
 #-}

module Example(Fonts(..), gui, guiModel) where

import Widget(Widget, WidgetFuncs(..))
import qualified Widgets.TextEdit as TextEdit
import qualified Widgets.TextView as TextView
import qualified Widgets.Proxy as Proxy
import qualified Widgets.Grid as Grid
import qualified Widgets.Box as Box
import qualified Widgets.Space as Space
import qualified Widgets.KeysTable as KeysTable
import HaskGame.Font(Font)
import Accessor(Accessor, accessor, aMapValue, (^>), (^.))
import HaskGame.Color(Color(..))
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

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

guiModel :: Model
guiModel =
    Model
    {
      vboxModel = Box.delegatedMutable False 0
    , hboxModel = Box.Mutable 0
    , textEditModels =
      Map.fromList [((x, y),
                     TextEdit.delegatedMutable False "Hello world" 5)
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

space :: Int -> Int -> Widget Model
space x y = Space.new (const $ Space.imm x y)

textEdit :: Grid.Cursor -> Fonts -> Widget Model
textEdit cursor fonts =
    TextEdit.newDelegated
                  (const $
                   TextEdit.imm
                           textEditingColor
                           textEditCursorColor
                           (defaultFont fonts)
                           textEditColor) $
                  atextEditModels ^> aMapValue cursor

grid, textView, hbox, vbox, keysTable, proxy :: Fonts -> Widget Model

grid fonts =
    Grid.newDelegated
       (const $ Grid.imm (2, 2) items)
       agridModel
    where
      items = Map.fromList
              [((x, y), Grid.Item (textEdit (x, y) fonts) (0.5, 1))
               | x <- [0..1], y <- [0..1]]

textView fonts =
    TextView.new
           (const $
            TextView.imm textViewColor (textViewFont fonts)
                         "This is just a view")

hbox fonts =
    Box.new
       (const $ Box.imm Box.Horizontal items) $
       ahboxModel
    where
      items = [Box.Item (vbox fonts) 0.5
              ,Box.Item (space 50 0) 0
              ,Box.Item (keysTable fonts) 0]

vbox fonts = Box.newDelegated
       (const $
        Box.imm
           Box.Vertical
              items) avboxModel
    where
      items = [Box.Item (grid fonts) 1
              ,Box.Item (space 0 100) 0.5
              ,Box.Item (textEdit (0, 1) fonts) 0.5
              ,Box.Item (textView fonts) 0.5
              ,Box.Item (proxy fonts) 0.5]

keysTable fonts = KeysTable.new
            (let handlers = fromMaybe Map.empty . widgetGetKeymap . hbox fonts
             in KeysTable.imm (keysFont fonts) (descFont fonts) . handlers)

proxy fonts = Proxy.new $ Proxy.imm .
        ([textEdit (1, 0) fonts,
          textView fonts]!!) .
        -- The index is chosen as the (1-vbox cursor)
        (1-) . min 1 .
        (^. (avboxModel ^> Box.aDelegatedMutableCursor))

gui :: Fonts -> Widget Model
gui = hbox
