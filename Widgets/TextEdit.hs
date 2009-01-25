{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.TextEdit where

import qualified MySDLKey
import MySDLKey(inKeyGroup, noMods, ctrl)
import qualified MySDLKeys
import qualified Widget
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Func(result)
import Data.Map((!))
import Graphics.UI.SDL.Keysym(SDLKey)
import Control.Arrow(second, (***))
import Vector2(Vector2(..))

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

data State = State {
  stateText :: String,
  stateCursor :: Int
}

data TextEdit = TextEdit {
  textEditColor :: SDL.Color
}

new :: SDL.Color -> String -> Int -> Widget.AnyWidgetState
new col str cursor = Widget.AnyWidgetState (TextEdit col) (State str cursor)

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

keysMap :: State -> Widget.Handlers State
keysMap state =
    Map.fromList . map (((,) Widget.KeyDown) *** second ($state)) $ actions

actions :: [(MySDLKeys.KeyGroup,
                     (String, State -> MySDLKey.Key -> State))]
actions =
    (MySDLKeys.printableGroup, ("Insert", insert)) :
    -- map . second . second . result gets to the result State
    -- of each of the following actions.  We apply const on it, so we
    -- get: MySDLKey.Key -> State instead:
    (map . second . second . result) const
    [(inKeyGroup noMods SDL.SDLK_BACKSPACE, actBackspace)
    ,(inKeyGroup ctrl   SDL.SDLK_h, actBackspace)
    ,(inKeyGroup noMods SDL.SDLK_DELETE, actDelete)
    ,(inKeyGroup ctrl   SDL.SDLK_d, actDelete)
    ,(inKeyGroup noMods SDL.SDLK_LEFT, actMovePrev)
    ,(inKeyGroup noMods SDL.SDLK_RIGHT, actMoveNext)
    ,(inKeyGroup noMods SDL.SDLK_HOME, actHome)
    ,(inKeyGroup ctrl   SDL.SDLK_a, actHome)
    ,(inKeyGroup noMods SDL.SDLK_END, actEnd)
    ,(inKeyGroup ctrl   SDL.SDLK_e, actEnd)
    ]

cursorWidth :: Int
cursorWidth = 2

cursorColor :: SDL.Color
cursorColor = SDL.Color 255 0 0

instance Widget.Widget TextEdit State where
    getKeymap _ = keysMap
    draw te (State text cursor) = do
      Vector2 w h <- Draw.computeToDraw . Draw.textSize $ take cursor text
      let cursorSize = Vector2 cursorWidth h
          cursorPos = Vector2 w 0
      size <- Draw.text (textEditColor te) text
      Draw.move cursorPos $ Draw.rect cursorColor cursorSize
      return size
    size _ (State text _) = Draw.textSize text
