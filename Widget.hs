{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
    -XFunctionalDependencies
  #-}

module Widget where

import qualified MySDLKey
import qualified Data.Map as Map
import Draw(Draw, Compute, Size)
import Accessor(Accessor)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

type KeyAction = (KeyStatus, MySDLKey.KeyGroup)
type Handler s = (String, MySDLKey.Key -> s)
type ActionHandlers s = Map.Map KeyAction (Handler s)

data DrawInfo = DrawInfo
    {
      diHasFocus :: Bool
    }
  deriving (Eq, Ord, Show, Read)

data Widget model = Widget
    {
      widgetDraw :: DrawInfo -> model -> Draw Size
    , widgetSize :: DrawInfo -> model -> Compute Size
    , widgetGetKeymap :: model -> Maybe (ActionHandlers model)
    }

type New model mutable =
    Accessor model mutable -> Widget model
