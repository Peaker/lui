{-# OPTIONS_GHC -Wall -XDeriveDataTypeable #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified Control.Exception as Exc
import qualified TextEdit
import qualified Widget
import qualified HierMap
import Data.Typeable(Typeable)
import Control.Monad(forM_, forever)

speed :: Num a => a
speed = 10

data QuitRequest = QuitRequest
  deriving (Typeable, Show)
instance Exc.Exception QuitRequest where

handleKeyAction :: Widget.Widget a => a -> Widget.KeyStatus -> SDL.Keysym -> IO ()
handleKeyAction widget keyStatus keySym = do
  keyMap <- Widget.getKeymap widget
  case HierMap.lookup (keyStatus, SDL.symKey keySym) keyMap of
    Nothing -> return ()
    Just (description, action) -> do
      putStrLn $ "Executing " ++ description
      action

handleEvents :: Widget.Widget a => a -> [SDL.Event] -> IO ()
handleEvents widget events = do
  forM_ events $ \event ->
      case event of 
        SDL.Quit -> Exc.throwIO QuitRequest
        SDL.KeyDown k -> handleKeyAction widget Widget.KeyDown k
        SDL.KeyUp k -> handleKeyAction widget Widget.KeyUp k
        _ -> return ()

mainLoop :: Widget.Widget a => a -> IO ()
mainLoop widget = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display (0, 0, 0)
  forever $ do
    SDL.fillRect display Nothing blackPixel
    Widget.draw widget (MySDL.makeRect $ MySDL.Vector2 0 0) display
    events <- MySDL.getEvents
    handleEvents widget events
    SDL.flip display
    ticks <- SDL.getTicks
    SDL.delay (speed - (ticks `mod` speed))

main :: IO ()
main = do
  MySDL.withSDL $ do
    let red = MySDL.sdlColor (255, 0, 0)
    textEdit <- TextEdit.new red 40 "Hello"
    flip Exc.catch errHandler (mainLoop textEdit)
    where
      errHandler :: QuitRequest -> IO ()
      errHandler = const . putStrLn $ "Quit requested"
