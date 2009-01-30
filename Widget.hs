{-# OPTIONS_GHC -Wall -O2
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

-- TODO: Consider moving the model argument outside of the record
data WidgetFuncs model = WidgetFuncs
    {
      widgetDraw :: DrawInfo -> Draw Size
    , widgetSize :: DrawInfo -> Compute Size
    , widgetGetKeymap :: Maybe (ActionHandlers model)
    }

type Widget model = model -> WidgetFuncs model

type NewImmutable model immutable =
    (model -> immutable) -> Widget model
type New model immutable mutable =
    (model -> immutable) -> Accessor model mutable -> Widget model
