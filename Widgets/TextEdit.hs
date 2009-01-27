{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextEdit where

import qualified Widget
import qualified MySDLKey
import MySDLKey(asKeyGroup, noMods, ctrl)
import qualified MySDLKeys
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import qualified Widgets.FocusDelegator as FocusDelegator
import Data.Map((!))
import Graphics.UI.SDL.Keysym(SDLKey)
import Control.Arrow(first, second)
import Vector2(Vector2(..))
import List(isSorted)

data TextEdit = TextEdit {
      textEditEditingBGColor :: SDL.Color
    , textEditColor :: SDL.Color
    , textEditCursorColor :: SDL.Color
    , textEditCursorWidth :: Int
}

data State = State {
      stateText :: String
    , stateCursor :: Int
}

type New w s = SDL.Color -> SDL.Color -> Int -> Int ->
               SDL.Color -> String -> Widget.WidgetState w s

new :: New TextEdit State
new editingBGColor cursorColor cursorWidth cursor textColor str =
    Widget.WidgetState (TextEdit editingBGColor textColor
                                 cursorColor cursorWidth)
                       (State str cursor)

newDelegated :: SDL.Color -> Bool ->
                New FocusDelegator.FocusDelegator FocusDelegator.State
newDelegated notEditingBGColor startEditing
             textColor str editingBGColor cursorColor cursorWidth cursor =
    FocusDelegator.new "Start editing" "Stop editing"
                       notEditingBGColor startEditing . Widget.upCast $
                       new textColor str editingBGColor cursorColor cursorWidth cursor

insert :: State -> MySDLKey.Key -> State
insert (State oldText oldCursor) key =
    let iText = MySDLKeys.keysUnicode!key
        (preOldText, postOldText) = splitAt oldCursor oldText
        newText = concat [preOldText, iText, postOldText]
        newCursor = oldCursor + length iText
    in State newText newCursor

delBackward :: Int -> State -> State
delBackward count (State oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPreText = take (length oldPreText - count) oldPreText
        newText = newPreText ++ oldPostText
        newCursor = length newPreText
    in State newText newCursor

delForward :: Int -> State -> State
delForward count (State oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPostText = drop count oldPostText
        newText = oldPreText ++ newPostText
    in State newText oldCursor

moveCursor :: (Int -> Int) -> State -> State
moveCursor cursorFunc (State text oldCursor) =
    let newCursor = cursorFunc oldCursor
    in State text $ if isSorted [0, newCursor, length text]
                            then newCursor
                            else oldCursor

goHome :: State -> State
goHome (State text _) = State text 0

goEnd :: State -> State
goEnd (State text _) = State text (length text)

actBackspace, actDelete, actMovePrev, actMoveNext, actHome, actEnd ::
    (String, State -> State)

actBackspace = ("Delete previous character", delBackward 1)
actDelete = ("Delete next character",        delForward 1)
actMovePrev = ("Move to previous character", moveCursor (subtract 1))
actMoveNext = ("Move to next character",     moveCursor (+1))
actHome = ("Move to beginning of text",      goHome)
actEnd = ("Move to end of text",             goEnd)

keysMap :: State -> Widget.ActionHandlers State
keysMap state = Map.fromList . (map . first) ((,) Widget.KeyDown) $
    (MySDLKeys.printableGroup, ("Insert", insert state)) :
    (map . second . second) (const . ($state)) (normalActions state ++ ctrlActions state)

cond :: Bool -> [a] -> [a]
cond p i = if p then i else []

normalActions :: State -> [(MySDLKey.KeyGroup, (String, State -> State))]
normalActions state =
    let cursor = stateCursor state
        text = stateText state
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

ctrlActions :: State -> [(MySDLKey.KeyGroup, (String, State -> State))]
ctrlActions state =
    let cursor = stateCursor state
        text = stateText state
    in (map . first) (asKeyGroup ctrl) . concat $
           [cond (cursor > 0)
                     [(SDL.SDLK_h, actBackspace)
                     ,(SDL.SDLK_a, actHome)]
           ,cond (cursor < length text)
                     [(SDL.SDLK_d, actDelete)
                     ,(SDL.SDLK_e, actEnd)]
           ]

instance Widget.Widget TextEdit State where
    getKeymap _ = Just . keysMap

    size _ _ (State text _) = Draw.textSize text

    draw drawInfo textEdit (State text cursor) = do
      if Widget.diHasFocus drawInfo
        then do
          textSize <- Draw.computeToDraw . Draw.textSize $ text
          Vector2 w h <- Draw.computeToDraw . Draw.textSize $ take cursor text
          let cursorSize = Vector2 (textEditCursorWidth textEdit) h
              cursorPos = Vector2 w 0
          Draw.rect (textEditEditingBGColor textEdit) textSize
          Draw.text (textEditColor textEdit) text
          Draw.move cursorPos $ Draw.rect (textEditCursorColor textEdit) cursorSize
          return textSize
        else
          Draw.text (textEditColor textEdit) text
