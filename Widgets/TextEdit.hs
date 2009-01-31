{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.TextEdit where

import qualified Widget
import Widget(WidgetFuncs(..))

import qualified HaskGame.Key as Key
import HaskGame.Key(asKeyGroup, noMods, ctrl)
import qualified HaskGame.Keys as Keys
import HaskGame.Vector2(Vector2(..))
import qualified HaskGame.Color as Color
import qualified Draw
import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map
import qualified Widgets.FocusDelegator as FocusDelegator
import Data.Map((!))
import Control.Arrow(first, second)
import Func(result)
import List(isSorted)
import Accessor((^.), (^>), write, afirst, asecond)

data Immutable = Immutable
    {
      immutableBgColor :: Color.Color
    , immutableCursorColor :: Color.Color
    , immutableCursorWidth :: Int
    , immutableFont :: Draw.Font
    , immutableTextColor :: Color.Color
    }

data Mutable = Mutable
    {
      mutableText :: String
    , mutableCursor :: Int
    }

insert :: Mutable -> Key.ModKey -> Mutable
insert (Mutable oldText oldCursor) key =
    let iText = Keys.keysUnicode!key
        (preOldText, postOldText) = splitAt oldCursor oldText
        newText = concat [preOldText, iText, postOldText]
        newCursor = oldCursor + length iText
    in Mutable newText newCursor

delBackward :: Int -> Mutable -> Mutable
delBackward count (Mutable oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPreText = take (length oldPreText - count) oldPreText
        newText = newPreText ++ oldPostText
        newCursor = length newPreText
    in Mutable newText newCursor

delForward :: Int -> Mutable -> Mutable
delForward count (Mutable oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPostText = drop count oldPostText
        newText = oldPreText ++ newPostText
    in Mutable newText oldCursor

moveCursor :: (Int -> Int) -> Mutable -> Mutable
moveCursor cursorFunc (Mutable text oldCursor) =
    let newCursor = cursorFunc oldCursor
    in Mutable text $ if isSorted [0, newCursor, length text]
                            then newCursor
                            else oldCursor

goHome :: Mutable -> Mutable
goHome (Mutable text _) = Mutable text 0

goEnd :: Mutable -> Mutable
goEnd (Mutable text _) = Mutable text (length text)

actBackspace, actDelete, actMovePrev, actMoveNext, actHome, actEnd ::
    (String, Mutable -> Mutable)

actBackspace = ("Delete previous character", delBackward 1)
actDelete = ("Delete next character",        delForward 1)
actMovePrev = ("Move to previous character", moveCursor (subtract 1))
actMoveNext = ("Move to next character",     moveCursor (+1))
actHome = ("Move to beginning of text",      goHome)
actEnd = ("Move to end of text",             goEnd)

keysMap :: Mutable -> Widget.ActionHandlers Mutable
keysMap mutable = Map.fromList . (map . first) ((,) Widget.KeyDown) $
    (Keys.printableGroup, ("Insert", insert mutable)) :
    (map . second . second) (const . ($mutable)) (normalActions mutable ++ ctrlActions mutable)

cond :: Bool -> [a] -> [a]
cond p i = if p then i else []

normalActions :: Mutable -> [(Key.KeyGroup, (String, Mutable -> Mutable))]
normalActions mutable =
    let cursor = mutableCursor mutable
        text = mutableText mutable
    in (map . first) (asKeyGroup noMods) . concat $
           [cond (cursor > 0)
                     [(SDL.SDLK_BACKSPACE, actBackspace)
                     ,(SDL.SDLK_LEFT, actMovePrev)
                     ,(SDL.SDLK_HOME, actHome)]
           ,cond (cursor < length text)
                     [(SDL.SDLK_DELETE, actDelete)
                     ,(SDL.SDLK_RIGHT, actMoveNext)
                     ,(SDL.SDLK_END, actEnd)]
           ]

ctrlActions :: Mutable -> [(Key.KeyGroup, (String, Mutable -> Mutable))]
ctrlActions mutable =
    let cursor = mutableCursor mutable
        text = mutableText mutable
    in (map . first) (asKeyGroup ctrl) . concat $
           [cond (cursor > 0)
                     [(SDL.SDLK_h, actBackspace)
                     ,(SDL.SDLK_a, actHome)]
           ,cond (cursor < length text)
                     [(SDL.SDLK_d, actDelete)
                     ,(SDL.SDLK_e, actEnd)]
           ]

new :: Widget.New model Immutable Mutable
new immutableMaker acc model =
  let Immutable bgColor cursorColor cursorWidth font textColor = immutableMaker model
      mutable@(Mutable text cursor) = model ^. acc
  in WidgetFuncs
  {
    widgetDraw = \drawInfo -> do
    
    if Widget.diHasFocus drawInfo
      then do
        textSize <- Draw.computeToDraw . Draw.textSize font $ text
        Vector2 w h <- Draw.computeToDraw . Draw.textSize font $ take cursor text
        let cursorSize = Vector2 cursorWidth h
            cursorPos = Vector2 w 0
        Draw.rect bgColor textSize
        Draw.text textColor font text
        Draw.move cursorPos $ Draw.rect cursorColor cursorSize
        return textSize
      else
        Draw.text textColor font text

  , widgetSize = \_ -> Draw.textSize font text

  , widgetGetKeymap =
    let applyToModel newMutable = acc `write` newMutable $ model
    in Just $
       (Map.map . second . result) applyToModel $ keysMap mutable
  }

newDelegated :: Widget.New model (Color.Color, Immutable) (FocusDelegator.Mutable, Mutable)
newDelegated immutableMaker acc model =
    let (focusColor, immutable) = immutableMaker model
        textEdit = new (const immutable) $ acc ^> asecond
        focusDelegatorImmutable = FocusDelegator.Immutable
                                  "Start editing" "Stop editing" textEdit focusColor
    in FocusDelegator.new (const $ focusDelegatorImmutable) (acc ^> afirst) model
