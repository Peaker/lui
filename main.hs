{-# OPTIONS_GHC -Wall -O2
 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified HaskGame
import qualified HaskGame.Font as Font
import qualified HaskGame.Key as Key
import qualified HaskGame.Keys as Keys
import HaskGame.Vector2(Vector2(..))
import HaskGame.Color(Color(..))
import qualified Draw
import qualified Control.Monad.State as State

import Example(Fonts(Fonts), gui, guiModel)

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

import qualified Data.Map as Map
import Control.Monad(forM, forM_, msum)
import Control.Monad.Trans(lift)

-- Commented out for 6.8's lack of the new Exception
-- import qualified Control.Exception as Exc
-- import Data.Typeable(Typeable)

speed :: Num a => a
speed = 30

-- Commented out for 6.8's lack of the new Exception
-- data QuitRequest = QuitRequest
--   deriving (Typeable, Show)
-- instance Exc.Exception QuitRequest where

handleKeyAction :: WidgetFuncs model ->
                   Widget.KeyStatus -> Key.Keysym -> Maybe model
handleKeyAction widgetFuncs keyStatus keySym =
  let key = Key.keyOfEvent keySym
      keyGroups = Keys.groupsOfKey key
      mKeyHandler = msum $ map lookupGroup keyGroups
      lookupGroup keyGroup = Map.lookup (keyStatus, keyGroup) =<<
                             widgetGetKeymap widgetFuncs
      runHandler (_, func) = func key
  in fmap runHandler mKeyHandler

handleEvents :: [HaskGame.Event] -> Widget model -> State.StateT model IO Bool
handleEvents events widget =
  fmap or $ forM events $ \event -> do
    model <- State.get
    let mNewModel = case event of
                      -- 6.8 exceptions :-(
                      -- lift $ Exc.throwIO QuitRequest
                      SDL.Quit -> error "Quit"
                      SDL.KeyDown k -> handleKeyAction (widget model) Widget.KeyDown k
                      SDL.KeyUp k -> handleKeyAction (widget model) Widget.KeyUp k
                      _ -> Nothing
    case mNewModel of
      Nothing ->
          return False
      Just newState -> do
          State.put newState
          return True

mainLoop :: Widget model -> model -> IO ()
mainLoop widget initModel = do
  display <- HaskGame.setVideoMode 800 600 16
  (`State.evalStateT` initModel) $
    forM_ (True:repeat False) $ \shouldDraw -> do
      events <- lift $ HaskGame.getEvents
      handledEvent <- handleEvents events widget
      model <- State.get
      lift $ do
        HaskGame.fillSurface display (Color 0 0 0)
        if handledEvent || shouldDraw
          then do
            -- forM_ (Map.assocs $ fromMaybe Map.empty $ Widget.getKeymap widget model) $
            --   \((_, group), (desc, _)) -> do
            --     print (MySDLKey.keyGroupName group, desc)
            let draw = widgetDraw (widget model) (Widget.DrawInfo True)
            Draw.render display (Vector2 0 0) draw
            SDL.flip display
          else
            SDL.delay 20

main :: IO ()
main =
  HaskGame.withInit $ do
    font <- Font.defaultFont 30
    textViewFont <- Font.defaultFont 15
    keysFont <- Font.defaultFont 25
    descFont <- Font.defaultFont 25
    -- Commented out for 6.8's lack of the new Exception
    -- flip Exc.catch errHandler runWidget
    -- where
    --   errHandler :: QuitRequest -> IO ()
    --   errHandler = const . putStrLn $ "Quit requested"
    mainLoop (gui $ Fonts font textViewFont keysFont descFont) guiModel
