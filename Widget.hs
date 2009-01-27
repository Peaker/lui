{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XExistentialQuantification
    -XRank2Types
  #-}

module Widget where

import qualified MySDLKey
import qualified Data.Map as Map
import Vector2(Vector2)
import Draw(Draw, Compute)

data KeyStatus = KeyDown | KeyUp
  deriving (Eq, Ord, Show, Read)

type Size = Vector2 Int
type KeyAction = (KeyStatus, MySDLKey.KeyGroup)
type Handler s = (String, MySDLKey.Key -> s)
type ActionHandlers s = Map.Map KeyAction (Handler s)

data DrawInfo = DrawInfo
    {
      diHasFocus :: Bool
    }
  deriving (Eq, Ord, Show, Read)

class Widget w s | w -> s where
    getKeymap :: w -> s -> Maybe (ActionHandlers s)
    draw      :: DrawInfo -> w -> s -> Draw Size
    size      :: DrawInfo -> w -> s -> Compute Size

data WidgetState w s = Widget w s => WidgetState w s
onWidgetState :: Widget w s => WidgetState w s -> (w -> s -> a) -> a
onWidgetState (WidgetState w s) func = func w s

data AnyWidgetState = forall w s. Widget w s => AnyWidgetState w s
onAnyWidgetState :: AnyWidgetState -> (forall w s. Widget w s => w -> s -> a) -> a
onAnyWidgetState (AnyWidgetState w s) func = func w s
