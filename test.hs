{-# OPTIONS_GHC -Wall -XDeriveDataTypeable -O2 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import qualified Control.Exception as Exc
import qualified TextEdit
import qualified Widget
import qualified HierMap
import Data.Typeable(Typeable)
import Control.Monad(forM, forM_)

speed :: Num a => a
speed = 30

data QuitRequest = QuitRequest
  deriving (Typeable, Show)
instance Exc.Exception QuitRequest where

handleKeyAction :: Widget.Widget a => a -> Widget.KeyStatus -> SDL.Keysym -> IO ()
handleKeyAction widget keyStatus keySym = do
  keyMap <- Widget.getKeymap widget
  let mods = MySDLKey.modsOf (SDL.symModifiers keySym)
  case HierMap.lookup (keyStatus, mods, SDL.symKey keySym) keyMap of
    Nothing -> return ()
    Just (_, action) -> do
      -- putStrLn $ "Executing " ++ description
      action

($>) :: Functor f => f a -> b -> f b
x $> y = fmap (const y) x

handleEvents :: Widget.Widget a => a -> [SDL.Event] -> IO Bool
handleEvents widget events = do
  fmap or $ forM events $ \event ->
      case event of
        SDL.Quit -> Exc.throwIO QuitRequest
        SDL.KeyDown k -> handleKeyAction widget Widget.KeyDown k $> True
        SDL.KeyUp k -> handleKeyAction widget Widget.KeyUp k $> True
        _ -> return False

mainLoop :: Widget.Widget a => a -> IO ()
mainLoop widget = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display (0, 0, 0)
  forM_ (True:repeat False) $ \shouldDraw -> do
    SDL.fillRect display Nothing blackPixel
    events <- MySDL.getEvents
    handledEvent <- handleEvents widget events
    if handledEvent || shouldDraw
      then do
        surf <- Widget.draw widget
        SDL.blitSurface surf Nothing display Nothing
        SDL.flip display
      else
        SDL.delay 10

main :: IO ()
main = do
  MySDL.withSDL $ do
    let col = MySDL.sdlColor (255, 255, 255)
    textEdit <- TextEdit.new col 40 "Hello"
    flip Exc.catch errHandler (mainLoop textEdit)
    where
      errHandler :: QuitRequest -> IO ()
      errHandler = const . putStrLn $ "Quit requested"
