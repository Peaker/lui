{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.FocusDelegator where

import qualified Widget
import MySDLKey(asKeyGroup, noMods)
import qualified MySDLKey
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Control.Arrow(second)
import Func(result)
import Data.Maybe(fromMaybe)

data FocusDelegator = FocusDelegator {
      delegatorFocusColor :: SDL.Color
    , delegatorStartStr :: String
    , delegatorStopStr :: String
}

data State = State {
      stateDelegateFocus :: Bool
    , stateWidget :: Widget.AnyWidgetState
}

new :: String -> String ->
       SDL.Color -> Bool -> Widget.AnyWidgetState ->
       Widget.WidgetState FocusDelegator State
new startStr stopStr focusColor initDelegateFocus widget =
    Widget.WidgetState (FocusDelegator focusColor startStr stopStr)
                       (State initDelegateFocus widget)

buildKeymap :: SDL.SDLKey -> String -> Bool -> State -> Widget.ActionHandlers State
buildKeymap key desc newDelegating (State _ widget) =
    Map.fromList
           [
            ((Widget.KeyDown, asKeyGroup noMods key),
             (desc, const $ State newDelegating widget))
           ]

delegatingKeyMap, nonDelegatingKeyMap :: String -> State -> Widget.ActionHandlers State
nonDelegatingKeyMap startStr = buildKeymap SDL.SDLK_RETURN startStr  True
delegatingKeyMap    stopStr  = buildKeymap SDL.SDLK_ESCAPE stopStr False

instance Widget.Widget FocusDelegator State where
    getKeymap (FocusDelegator _ startStr stopStr)
              state@(State delegating (Widget.AnyWidgetState child oldChildState)) =
      Just $
      case delegating of
        False -> nonDelegatingKeyMap startStr state
        True -> let childKeys = fromMaybe Map.empty $
                                  (Map.map . second . result)
                                  (State delegating . Widget.AnyWidgetState child)
                                  `fmap`
                                  Widget.getKeymap child oldChildState
                in childKeys `Map.union` delegatingKeyMap stopStr state

    size drawInfo _ (State _ widget)  =
        Widget.onAnyWidgetState widget $ Widget.size drawInfo

    draw drawInfo (FocusDelegator focusColor _ _) (State delegating widget) = do
        let haveFocus = Widget.diHasFocus drawInfo && not delegating
            childDrawInfo = Widget.DrawInfo $
                            Widget.diHasFocus drawInfo && delegating
        if haveFocus
          then do
            size <- Draw.computeToDraw $
                    Widget.onAnyWidgetState widget $
                    Widget.size childDrawInfo
            Draw.rect focusColor size
            return ()
          else
            return ()
        Widget.onAnyWidgetState widget $ Widget.draw childDrawInfo
