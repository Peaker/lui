{-# OPTIONS_GHC -Wall -XDeriveDataTypeable -O2 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import qualified MySDLKeys
import qualified Draw
import qualified Control.Exception as Exc
import qualified Control.Monad.State as State
import qualified TextEdit
import qualified Widget
import qualified Data.Map as Map
import Data.Typeable(Typeable)
import Control.Monad(forM, forM_, msum)
import Control.Monad.Trans(lift)

speed :: Num a => a
speed = 30

data QuitRequest = QuitRequest
  deriving (Typeable, Show)
instance Exc.Exception QuitRequest where

handleKeyAction :: Widget.Widget a s =>
                   a -> Widget.KeyStatus -> SDL.Keysym -> s -> Maybe s
handleKeyAction widget keyStatus keySym state =
  let keyMap = Widget.getKeymap widget state
      key = MySDLKey.keyOfEvent keySym
      keyGroups = MySDLKeys.groupsOfKey key
      mKeyHandler = msum $ flip map keyGroups lookupGroup
      lookupGroup keyGroup = Map.lookup (keyStatus, keyGroup) keyMap
      runHandler (_, func) = func key
  in fmap runHandler mKeyHandler

maybeModify :: State.MonadState s m => (s -> Maybe s) -> m Bool
maybeModify f = do
  state <- State.get
  maybe (return False) updateState (f state)
  where
    updateState s = do
      State.put s
      return True

handleEvents :: Widget.Widget a s => a -> [SDL.Event] -> State.StateT s IO Bool
handleEvents widget events =
  fmap or $ forM events $ \event ->
      case event of
        SDL.Quit -> lift $ Exc.throwIO QuitRequest
        SDL.KeyDown k -> maybeModify $
                         handleKeyAction widget Widget.KeyDown k
        SDL.KeyUp k -> maybeModify $
                       handleKeyAction widget Widget.KeyUp k
        _ -> return False

mainLoop :: Widget.Widget a s => a -> s -> IO ()
mainLoop widget initState = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display (0, 0, 0)
  font <- MySDL.defaultFont 30
  flip State.evalStateT initState $
    forM_ (True:repeat False) $ \shouldDraw -> do
      lift $ SDL.fillRect display Nothing blackPixel
      events <- lift $ MySDL.getEvents
      handledEvent <- handleEvents widget events
      state <- State.get
      lift $
        if handledEvent || shouldDraw
        then do
          -- forM_ (Map.assocs (Widget.getKeymap widget state)) $
          --   \((_, group), (desc, _)) -> do
          --     print (MySDLKey.keyGroupName group, desc)
          let draw = Widget.draw widget state
          Draw.render font display (fromInteger 0) draw
          SDL.flip display
        else
          SDL.delay 20

main :: IO ()
main = do
  MySDL.withSDL $ do
    let col = MySDL.sdlColor (255, 255, 255)
        textEdit = TextEdit.TextEdit col
        textEditState = TextEdit.TextEditState "Hello" 5
    flip Exc.catch errHandler (mainLoop textEdit textEditState)
    where
      errHandler :: QuitRequest -> IO ()
      errHandler = const . putStrLn $ "Quit requested"
