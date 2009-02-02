{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widget where

import qualified Graphics.UI.HaskGame.Key as Key
import qualified Data.Map as Map
import Graphics.UI.LUI.Draw(Draw, Compute, Size)
import Graphics.UI.LUI.Accessor(Accessor)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

type KeyAction = (KeyStatus, Key.KeyGroup)
type Handler model = (String, Key.ModKey -> model)
type ActionHandlers model = Map.Map KeyAction (Handler model)

data DrawInfo = DrawInfo
    {
      diHasFocus :: Bool
    }
  deriving (Eq, Ord, Show, Read)

-- TODO: Consider moving the model argument outside of the record
data WidgetFuncs model = WidgetFuncs
    {
      widgetDraw :: DrawInfo -> Draw Size
    , widgetSize :: DrawInfo -> Compute Size
    , widgetGetKeymap :: Maybe (ActionHandlers model)
    }

type Widget model = model -> WidgetFuncs model

type New model mutable =
    Accessor model mutable -> Widget model