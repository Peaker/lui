{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Run(mainLoop) where

import qualified Graphics.UI.LUI.Image as Image
import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.HaskGame as HaskGame
import qualified Graphics.UI.HaskGame.Key as Key
import qualified Graphics.UI.HaskGame.Keys as Keys
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Graphics.UI.HaskGame.Color(Color(..))

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Control.Monad(forM, forM_, msum)
import Control.Monad.Trans(lift)
import Control.Monad.Maybe(MaybeT(..))

handleKeyAction :: WidgetFuncs model ->
                   Widget.KeyStatus -> Key.Keysym ->
                   Maybe model
handleKeyAction widgetFuncs keyStatus keySym =
  let key = Key.keyOfEvent keySym
      keyGroups = Keys.groupsOfKey key
      mKeyHandler = msum $ map lookupGroup keyGroups
      lookupGroup keyGroup = Map.lookup (keyStatus, keyGroup) =<<
                             widgetGetKeymap widgetFuncs
      runHandler (_, func) = func key
  in fmap runHandler mKeyHandler

handleEvent :: HaskGame.Event -> WidgetFuncs model -> Maybe model
handleEvent event widgetFuncs =
    case event of
      SDL.KeyDown k -> handleKeyAction widgetFuncs Widget.KeyDown k
      SDL.KeyUp k -> handleKeyAction widgetFuncs Widget.KeyUp k
      _ -> Nothing

handleEvents :: [HaskGame.Event] -> Widget model ->
                MaybeT (State.StateT model IO) Bool
handleEvents events widget =
  fmap or $ forM events $ \event -> do
    model <- lift $ State.get
    mNewModel <-
        case event of
          SDL.Quit -> fail "Quit"
          _ -> return . handleEvent event $ widget model
    case mNewModel of
      Nothing ->
          return False
      Just newModel -> do
          lift $ State.put newModel
          return True

mainLoop :: Widget model -> model -> IO model
mainLoop widget initModel = do
  display <- HaskGame.setVideoMode (Vector2 800 600) 16
  (`State.execStateT` initModel) . runMaybeT $ do
    forM_ (True:repeat False) $ \shouldDraw -> do
      events <- lift . lift $ HaskGame.getEvents
      handledEvent <- handleEvents events widget
      model <- State.get
      lift . lift $ do
        HaskGame.fillSurface display (Color 0 0 0)
        if handledEvent || shouldDraw
          then do
            let draw = widgetImage (widget model) (Widget.DrawInfo True)
            Image.render draw display $ Vector2 0 0
            SDL.flip display
          else
            SDL.delay 20
