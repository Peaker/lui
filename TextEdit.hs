{-# OPTIONS_GHC -Wall -XDeriveDataTypeable #-}

module TextEdit where

import qualified MySDL
import qualified Widget
import qualified HierMap
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym(SDLKey)
import qualified Data.Map as Map
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Control.Arrow(second)

data TextEditState = TextEditState {
  textEditStateText :: String,
  textEditStateCursor :: Int
}

data TextEdit = TextEdit {
  textEditColor :: SDL.Color,
  textEditFontSize :: Int,
  textEditStateRef :: IORef TextEditState
}

strOf :: SDLKey -> String
strOf SDL.SDLK_a = "a"
strOf SDL.SDLK_b = "b"
strOf _ = "_"

insert :: String -> TextEdit -> IO ()
insert iText te = modifyIORef (textEditStateRef te) insertText
    where insertText (TextEditState oldText oldCursor) =
              let (preOldText, postOldText) = splitAt oldCursor oldText
                  newText = concat [preOldText, iText, postOldText]
                  newCursor = oldCursor + length iText
              in TextEditState newText newCursor

backspace :: TextEdit -> IO ()
backspace te = modifyIORef (textEditStateRef te) deleteText
    where deleteText (TextEditState oldText oldCursor) =
              let (preOldText, postOldText) = splitAt oldCursor oldText
                  newPreText = init preOldText
                  newText = concat [newPreText, postOldText]
                  newCursor = length newPreText
              in TextEditState newText newCursor

textEditKeysMap :: Map.Map (Widget.KeyStatus, SDLKey) (String, TextEdit -> IO ())
textEditKeysMap = Map.fromList $
  ((Widget.KeyDown, SDL.SDLK_BACKSPACE),
   ("Delete previous character", backspace)) :
  [
   ((Widget.KeyDown, x), let s = (strOf x)
       in ("Insert " ++ s, insert s))
   | x <- [SDL.SDLK_a, SDL.SDLK_b]
  ]

instance Widget.Widget TextEdit where
    getKeymap w = return . HierMap.simpleHierMap $ Map.map (second ($w)) textEditKeysMap
    draw te r surf = do
      font <- MySDL.defaultFont (textEditFontSize te)
      state <- readIORef (textEditStateRef te)
      textSurface <- MySDL.renderTextSolid font
                     (textEditStateText state) (textEditColor te)
      SDL.blitSurface textSurface Nothing surf (Just r)
      return ()

new :: SDL.Color -> Int -> String -> IO TextEdit
new color fontSize initText = do
  textState <- newIORef (TextEditState initText (length initText))
  return $ TextEdit color fontSize textState