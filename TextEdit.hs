{-# OPTIONS_GHC -Wall -O2 #-}

module TextEdit where

import qualified MySDL
import qualified Widget
import qualified HierMap
import qualified Graphics.UI.SDL as SDL
import qualified MySDLKey
import Vector2(Vector2(..), vector2first)
import Graphics.UI.SDL.Keysym(SDLKey)
import qualified Data.Map as Map
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.Maybe(catMaybes)
import Control.Arrow(second)

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

data TextEditState = TextEditState {
  textEditStateText :: String,
  textEditStateCursor :: Int
}

data TextEdit = TextEdit {
  textEditColor :: SDL.Color,
  textEditFontSize :: Int,
  textEditStateRef :: IORef TextEditState
}

insert :: String -> TextEditState -> TextEditState
insert iText (TextEditState oldText oldCursor) =
    let (preOldText, postOldText) = splitAt oldCursor oldText
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

type TextEditAction = (String, TextEdit -> IO ())

textEditAction :: String -> (TextEditState -> TextEditState) -> TextEditAction
textEditAction description func = (description, act)
    where act te = modifyIORef (textEditStateRef te) func

actBackspace, actDelete, actMovePrev, actMoveNext, actHome, actEnd :: TextEditAction
actBackspace = textEditAction "Delete previous character" $ delBackward 1
actDelete = textEditAction "Delete next character" $ delForward 1
actMovePrev = textEditAction "Move to previous character" $ moveCursor (subtract 1)
actMoveNext = textEditAction "Move to next character" $ moveCursor (+1)
actHome = textEditAction "Move to beginning of text" goHome
actEnd = textEditAction "Move to end of text" goEnd

normKey, ctrlKey :: SDLKey -> (Widget.KeyStatus, MySDLKey.Mods, SDLKey)
normKey key = (Widget.KeyDown, MySDLKey.noMods, key)
ctrlKey key = (Widget.KeyDown, MySDLKey.ctrl, key)

textEditKeysMap :: Map.Map (Widget.KeyStatus, MySDLKey.Mods, SDLKey) TextEditAction
textEditKeysMap = Map.fromList $
  (normKey SDL.SDLK_BACKSPACE, actBackspace) :
  (ctrlKey SDL.SDLK_h, actBackspace) :

  (normKey SDL.SDLK_DELETE, actDelete) :
  (ctrlKey SDL.SDLK_d, actDelete) :

  (normKey SDL.SDLK_LEFT, actMovePrev) :
  (normKey SDL.SDLK_RIGHT, actMoveNext) :

  (normKey SDL.SDLK_HOME, actHome) :
  (ctrlKey SDL.SDLK_a, actHome) :

  (normKey SDL.SDLK_END, actEnd) :
  (ctrlKey SDL.SDLK_e, actEnd) :

  catMaybes [
   insertableKeyHandler key mods
   `fmap` MySDLKey.strOf mods key
   | key <- MySDL.allValues
   , mods <- [MySDLKey.noMods, MySDLKey.shift]
  ]
    where insertableKeyHandler key mods str =
              ((Widget.KeyDown, mods, key), textEditAction ("Insert " ++ str) . insert $ str)

cursorWidth :: Int
cursorWidth = 2

cursorColor :: MySDL.Color
cursorColor = (255, 0, 0)

instance Widget.Widget TextEdit where
    getKeymap w = return . HierMap.simpleHierMap $ Map.map (second ($w)) textEditKeysMap
    draw te = do
      font <- MySDL.defaultFont (textEditFontSize te)
      state <- readIORef (textEditStateRef te)
      let text = textEditStateText state
          cursor = textEditStateCursor state
          preText = take cursor text

      Vector2 preTextWidth preTextHeight <- MySDL.textSize font preText

      textSurface <- MySDL.renderText font text (textEditColor te)

      let cursorRect = MySDL.makeRect (Vector2 preTextWidth 0)
                                      (Vector2 cursorWidth preTextHeight)

      resultSurface <- MySDL.createRGBSurface $
                       vector2first (+cursorWidth) (MySDL.surfaceSize textSurface)
      SDL.blitSurface textSurface Nothing resultSurface Nothing
      cursorPixelColor <- MySDL.sdlPixel resultSurface cursorColor
      SDL.fillRect resultSurface (Just cursorRect) cursorPixelColor

      return resultSurface

new :: SDL.Color -> Int -> String -> IO TextEdit
new color fontSize initText = do
  textState <- newIORef (TextEditState initText (length initText))
  return $ TextEdit color fontSize textState
