{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module TextEdit where

import qualified MySDL
import qualified MySDLKey
import qualified MySDLKeys
import qualified Widget
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Data.Map((!))
import Graphics.UI.SDL.Keysym(SDLKey)
import Control.Arrow(second)
import Vector2(Vector2(..))
import Control.Arrow(first)

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

data TextEditState = TextEditState {
  textEditStateText :: String,
  textEditStateCursor :: Int
}

data TextEdit = TextEdit {
  textEditColor :: SDL.Color
}

insert :: TextEditState -> MySDLKey.Key -> TextEditState
insert (TextEditState oldText oldCursor) key =
    let iText = MySDLKeys.keysUnicode!key
        (preOldText, postOldText) = splitAt oldCursor oldText
        newText = concat [preOldText, iText, postOldText]
        newCursor = oldCursor + length iText
    in TextEditState newText newCursor

delBackward :: Int -> TextEditState -> TextEditState
delBackward count (TextEditState oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPreText = take (length oldPreText - count) oldPreText
        newText = newPreText ++ oldPostText
        newCursor = length newPreText
    in TextEditState newText newCursor

delForward :: Int -> TextEditState -> TextEditState
delForward count (TextEditState oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPostText = drop count oldPostText
        newText = oldPreText ++ newPostText
    in TextEditState newText oldCursor

moveCursor :: (Int -> Int) -> TextEditState -> TextEditState
moveCursor cursorFunc (TextEditState text oldCursor) =
    let newCursor = cursorFunc oldCursor
    in TextEditState text $ if isSorted [0, newCursor, length text]
                            then newCursor
                            else oldCursor

goHome :: TextEditState -> TextEditState
goHome (TextEditState text _) = TextEditState text 0

goEnd :: TextEditState -> TextEditState
goEnd (TextEditState text _) = TextEditState text (length text)

type TextEditAction = (String, TextEditState -> MySDLKey.Key -> TextEditState)

actBackspace, actDelete, actMovePrev, actMoveNext, actHome, actEnd :: TextEditAction
actBackspace = ("Delete previous character", const . delBackward 1)
actDelete = ("Delete next character",        const . delForward 1)
actMovePrev = ("Move to previous character", const . moveCursor (subtract 1))
actMoveNext = ("Move to next character",     const . moveCursor (+1))
actHome = ("Move to beginning of text",      const . goHome)
actEnd = ("Move to end of text",             const . goEnd)

normKey, ctrlKey :: SDLKey -> MySDLKey.KeyGroup
normKey key = MySDLKey.singletonKeyGroup $ MySDLKey.Key MySDLKey.noMods key
ctrlKey key = MySDLKey.singletonKeyGroup $ MySDLKey.Key MySDLKey.ctrl key

textEditKeysMap :: Map.Map (Widget.KeyStatus, MySDLKey.KeyGroup) TextEditAction
textEditKeysMap = Map.fromList . (map . first) ((,) Widget.KeyDown) $
                  [(normKey SDL.SDLK_BACKSPACE, actBackspace)
                  ,(ctrlKey SDL.SDLK_h, actBackspace)
                  ,(normKey SDL.SDLK_DELETE, actDelete)
                  ,(ctrlKey SDL.SDLK_d, actDelete)
                  ,(normKey SDL.SDLK_LEFT, actMovePrev)
                  ,(normKey SDL.SDLK_RIGHT, actMoveNext)
                  ,(normKey SDL.SDLK_HOME, actHome)
                  ,(ctrlKey SDL.SDLK_a, actHome)
                  ,(normKey SDL.SDLK_END, actEnd)
                  ,(ctrlKey SDL.SDLK_e, actEnd)
                  ,(MySDLKeys.printableGroup, ("Insert", insert))
                  ]

cursorWidth :: Int
cursorWidth = 2

cursorColor :: MySDL.Color
cursorColor = (255, 0, 0)

instance Widget.Widget TextEdit TextEditState where
    getKeymap _ s = Map.map (second ($s)) textEditKeysMap
    draw te (TextEditState text cursor) = do
        Vector2 w h <- Draw.textSize (take cursor text)
        let cursorSize = Vector2 cursorWidth h
            cursorPos = Vector2 w 0
        Draw.move cursorPos $ Draw.rect cursorColor cursorSize
        Draw.text (textEditColor te) text
