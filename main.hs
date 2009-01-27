{-# OPTIONS_GHC -Wall -XDeriveDataTypeable -O2 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import qualified MySDLKeys
import qualified Draw
import qualified Control.Exception as Exc
import qualified Control.Monad.State as State
import qualified Widget
import qualified Widgets.TextEdit as TextEdit
import qualified Widgets.Grid as Grid
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

mainLoop :: Widget.AnyWidgetState -> IO ()
mainLoop (Widget.AnyWidgetState widget initState) = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display $ SDL.Color 0 0 0
  font <- MySDL.defaultFont 30
  (`State.evalStateT` initState) $
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
          let draw = Widget.draw (Widget.DrawInfo True) widget state
          Draw.render font display (fromInteger 0) draw
          SDL.flip display
        else
          SDL.delay 20

main :: IO ()
main = do
  MySDL.withSDL $ do
    let textEditingColor = SDL.Color 30 20 100
        textEditColor = SDL.Color 255 255 255
        textEditCursorColor = SDL.Color 255 0 0
        focusColor = SDL.Color 0 0 150
        grid = Grid.newDelegated focusColor False (0, 0) (2, 2) $
               Map.fromList
               [((x, y),
                 Grid.Item (0.5, 1) $
                 TextEdit.newDelegated focusColor False textEditingColor textEditColor textEditCursorColor ("Hello " ++ show (x, y)) (x+y*2))
                | x <- [0..1]
                , y <- [0..1]]
        -- textEdit = TextEdit.newDelegated focusColor False textEditingColor
        --            textEditColor "Hello world" 5
        widget = grid

    flip Exc.catch errHandler (mainLoop widget)
    where
      errHandler :: QuitRequest -> IO ()
      errHandler = const . putStrLn $ "Quit requested"
