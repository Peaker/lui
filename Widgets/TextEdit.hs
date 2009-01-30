{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextEdit where

import qualified MySDLKey
import MySDLKey(asKeyGroup, noMods, ctrl)

import qualified Widget
import Widget(Widget(..))

import qualified MySDLKeys
import qualified Draw
import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map
-- import qualified Widgets.FocusDelegator as FocusDelegator
import Data.Map((!))
import Graphics.UI.SDL.Keysym(SDLKey)
import Control.Arrow(first, second)
import Func(result)
import Vector2(Vector2(..))
import List(isSorted)
import Accessor((^.), (^:))

data Mutable = Mutable {
      mutableText :: String
    , mutableCursor :: Int
}

insert :: Mutable -> MySDLKey.Key -> Mutable
insert (Mutable oldText oldCursor) key =
    let iText = MySDLKeys.keysUnicode!key
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
    (MySDLKeys.printableGroup, ("Insert", insert mutable)) :
    (map . second . second) (const . ($mutable)) (normalActions mutable ++ ctrlActions mutable)

cond :: Bool -> [a] -> [a]
cond p i = if p then i else []

normalActions :: Mutable -> [(MySDLKey.KeyGroup, (String, Mutable -> Mutable))]
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

ctrlActions :: Mutable -> [(MySDLKey.KeyGroup, (String, Mutable -> Mutable))]
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

type New model mutable =
    SDL.Color -> SDL.Color -> Int ->
    SDL.Color -> Widget.New model mutable

new :: New model Mutable
new editingBGColor cursorColor cursorWidth textColor accessor =
  Widget
  {
    widgetDraw = \drawInfo model -> do
    let Mutable text cursor = model ^. accessor
    if Widget.diHasFocus drawInfo
      then do
        textSize <- Draw.computeToDraw . Draw.textSize $ text
        Vector2 w h <- Draw.computeToDraw . Draw.textSize $ take cursor text
        let cursorSize = Vector2 cursorWidth h
            cursorPos = Vector2 w 0
        Draw.rect editingBGColor textSize
        Draw.text textColor text
        Draw.move cursorPos $ Draw.rect cursorColor cursorSize
        return textSize
      else
        Draw.text textColor text

  , widgetSize = \_ model ->
    let (Mutable text _) = (model ^. accessor)
    in Draw.textSize text

  , widgetGetKeymap = \model ->
    let mutable = model ^. accessor
        newModel newMutable = accessor ^: const newMutable $ model
    in Just $
       (Map.map . second . result) newModel $ keysMap mutable
  }

-- newDelegated :: SDL.Color -> Bool ->
--                 New (FocusDelegator.FocusDelegatorMutable TextEdit Mutable)
-- newDelegated notEditingBGColor startEditing
--              textColor str editingBGColor cursorColor cursorWidth cursor =
--     FocusDelegator.new "Start editing" "Stop editing"
--                        notEditingBGColor startEditing $
--                        new textColor str editingBGColor cursorColor cursorWidth cursor
