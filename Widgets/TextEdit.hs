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
import Func(result)
import Data.Map((!))
import Graphics.UI.SDL.Keysym(SDLKey)
import Control.Arrow(second, (***))
import Vector2(Vector2(..))

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

data State = State {
      stateText :: String
    , stateCursor :: Int
}

data TextEdit = TextEdit {
      textEditEditingBGColor :: SDL.Color
    , textEditColor :: SDL.Color
}

new :: SDL.Color -> SDL.Color -> String -> Int -> Widget.AnyWidgetState
new editingBGColor textColor str cursor = Widget.AnyWidgetState
                                  (TextEdit editingBGColor textColor) (State str cursor)

newDelegated :: SDL.Color -> Bool -> SDL.Color -> SDL.Color -> String -> Int ->
                Widget.AnyWidgetState
newDelegated notEditingBGColor startEditing editingBGColor textColor str cursor =
    FocusDelegator.new "Start editing" "Stop editing"
                       notEditingBGColor startEditing $
                       new editingBGColor textColor str cursor

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

actions :: [(MySDLKeys.KeyGroup,
             (String, State -> MySDLKey.Key -> State))]
actions =
    (MySDLKeys.printableGroup, ("Insert", insert)) :

    map (asKeyGroup noMods *** ignoreKey)
    [(SDL.SDLK_BACKSPACE, actBackspace)
    ,(SDL.SDLK_DELETE, actDelete)
    ,(SDL.SDLK_LEFT, actMovePrev)
    ,(SDL.SDLK_RIGHT, actMoveNext)
    ,(SDL.SDLK_HOME, actHome)
    ,(SDL.SDLK_END, actEnd)] ++

    map (asKeyGroup ctrl *** ignoreKey)
    [(SDL.SDLK_h, actBackspace)
    ,(SDL.SDLK_d, actDelete)
    ,(SDL.SDLK_a, actHome)
    ,(SDL.SDLK_e, actEnd)
    ]
    where
      -- ignoreKey adds an ignored (MySDLKey.Key ->) to the
      -- result State in the handlers
      ignoreKey = (second . result) const

keysMap :: State -> Widget.ActionHandlers State
keysMap state =
    Map.fromList . map (((,) Widget.KeyDown) *** second ($state)) $ actions

cursorWidth :: Int
cursorWidth = 2

cursorColor :: SDL.Color
cursorColor = SDL.Color 255 0 0

instance Widget.Widget TextEdit State where
    getKeymap _ = keysMap

    size _ _ (State text _) = Draw.textSize text

    draw drawInfo textEdit (State text cursor) = do
      Vector2 w h <- Draw.computeToDraw . Draw.textSize $ take cursor text
      textSize <- Draw.computeToDraw . Draw.textSize $ text
      let cursorSize = Vector2 cursorWidth h
          cursorPos = Vector2 w 0
      if Widget.diHasFocus drawInfo
        then do
          Draw.rect (textEditEditingBGColor textEdit) textSize
          Draw.text (textEditColor textEdit) text
          Draw.move cursorPos $ Draw.rect cursorColor cursorSize
          return textSize
        else
          Draw.text (textEditColor textEdit) text
