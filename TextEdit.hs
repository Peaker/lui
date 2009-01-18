{-# OPTIONS_GHC -Wall #-}

module TextEdit where

import qualified MySDL
import qualified Widget
import qualified HierMap
import qualified Graphics.UI.SDL as SDL
import qualified MySDLKey
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

backspace :: TextEditState -> TextEditState
backspace (TextEditState oldText oldCursor) =
    let (preOldText, postOldText) = splitAt oldCursor oldText
        newPreText = take (length preOldText - 1) preOldText
        newText = concat [newPreText, postOldText]
        newCursor = length newPreText
    in TextEditState newText newCursor

moveCursor :: (Int -> Int) -> TextEditState -> TextEditState
moveCursor cursorFunc (TextEditState text oldCursor) =
    let newCursor = cursorFunc oldCursor
    in TextEditState text $ if isSorted [0, newCursor, length text]
                            then newCursor
                            else oldCursor

textEditAction :: (TextEditState -> TextEditState) -> TextEdit -> IO ()
textEditAction func te = modifyIORef (textEditStateRef te) func

textEditKeysMap :: Map.Map (Widget.KeyStatus, SDLKey) (String, TextEdit -> IO ())
textEditKeysMap = Map.fromList $
  ((Widget.KeyDown, SDL.SDLK_BACKSPACE),
   ("Delete previous character", textEditAction backspace)) :
  ((Widget.KeyDown, SDL.SDLK_LEFT),
   ("Move to previous character", textEditAction . moveCursor $ (subtract 1))) :
  ((Widget.KeyDown, SDL.SDLK_RIGHT),
   ("Move to next character", textEditAction . moveCursor $ (+1))) :
  catMaybes [
   insertableKeyHandler key
   `fmap` MySDLKey.strOf key
   | key <- MySDLKey.allValues
  ]
    where insertableKeyHandler key str =
              ((Widget.KeyDown, key),
               (("Insert "++str), textEditAction . insert $ str))

cursorWidth :: Int
cursorWidth = 2

cursorColor :: MySDL.Color
cursorColor = (255, 0, 0)

instance Widget.Widget TextEdit where
    getKeymap w = return . HierMap.simpleHierMap $ Map.map (second ($w)) textEditKeysMap
    draw te pos surf = do
      font <- MySDL.defaultFont (textEditFontSize te)
      state <- readIORef (textEditStateRef te)
      let text = textEditStateText state
          cursor = textEditStateCursor state
          (preText, postText) = splitAt cursor text

      preTextSurface <- MySDL.renderTextSolid font
                        preText (textEditColor te)
      postTextSurface <- MySDL.renderTextSolid font
                         postText (textEditColor te)
      cursorPixelColor <- MySDL.sdlPixel surf cursorColor

      let (preTextWidth, preTextHeight) = MySDL.surfaceGetSize preTextSurface

      let cursorJRect = Just $ MySDL.makeSizedRect (MySDL.vectorX (preTextWidth+) pos)
                                                   (MySDL.Vector2 cursorWidth preTextHeight)

          leftJRect = Just . MySDL.makePosRect $ pos
          rightJRect = (fmap . MySDL.rectX) (preTextWidth+cursorWidth+) leftJRect
      SDL.blitSurface preTextSurface Nothing surf leftJRect
      SDL.fillRect surf cursorJRect cursorPixelColor
      SDL.blitSurface postTextSurface Nothing surf rightJRect
      return ()

new :: SDL.Color -> Int -> String -> IO TextEdit
new color fontSize initText = do
  textState <- newIORef (TextEditState initText (length initText))
  return $ TextEdit color fontSize textState