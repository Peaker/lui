{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Run(keyEvent, draw) where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(WidgetFuncs(..))

import qualified Graphics.UI.LUI.Keymap as Keymap

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))

keyEvent :: Keymap.ModKey -> WidgetFuncs model -> Maybe model
keyEvent key widgetFuncs = (snd . snd) `fmap` (Keymap.lookup key =<< widgetGetKeymap widgetFuncs)

draw :: WidgetFuncs model -> IO ()
draw widgetFuncs = Draw.clearRender $ Draw.translate (-1, -1) %% widgetImage widgetFuncs (Widget.DrawInfo True)
