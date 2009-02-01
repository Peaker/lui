{-# OPTIONS_GHC -Wall -O2
  #-}

module Widget where

import qualified HaskGame.Key as Key
import qualified Data.Map as Map
import Draw(Draw, Compute, Size)
import Accessor(Accessor)

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

type NewImmutable model immutable =
    (model -> immutable) -> Widget model
type New model immutable mutable =
    Accessor model mutable -> (model -> immutable) -> Widget model
