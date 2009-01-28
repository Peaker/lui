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

data FocusDelegator w s = FocusDelegator {
      delegatorFocusColor :: SDL.Color
    , delegatorStartStr :: String
    , delegatorStopStr :: String
}

data State w s = State {
      stateDelegateFocus :: Bool
    , stateWidget :: Widget.WidgetState w s
}

type FocusDelegatorState w s = Widget.WidgetState (FocusDelegator w s) (State w s)
type New w s r = String -> String -> SDL.Color -> Bool ->
                 Widget.WidgetState w s -> r

new :: Widget.Widget w s => New w s (FocusDelegatorState w s)
new startStr stopStr focusColor initDelegateFocus widget =
    Widget.WidgetState (FocusDelegator focusColor startStr stopStr)
                       (State initDelegateFocus widget)

buildKeymap :: Widget.Widget w s =>
               SDL.SDLKey -> String -> Bool ->
               State w s -> Widget.ActionHandlers (State w s)
buildKeymap key desc newDelegating (State _ widget) =
    Map.fromList
           [
            ((Widget.KeyDown, asKeyGroup noMods key),
             (desc, const $ State newDelegating widget))
           ]

delegatingKeyMap, nonDelegatingKeyMap ::
    Widget.Widget w s =>
    String -> State w s -> Widget.ActionHandlers (State w s)
nonDelegatingKeyMap startStr = buildKeymap SDL.SDLK_RETURN startStr  True
delegatingKeyMap    stopStr  = buildKeymap SDL.SDLK_ESCAPE stopStr False

instance Widget.Widget w s => Widget.Widget (FocusDelegator w s) (State w s) where
    getKeymap (FocusDelegator _ startStr stopStr)
              state@(State delegating (Widget.WidgetState child oldChildState)) =
      flip fmap (Widget.getKeymap child oldChildState) $ \childKeys ->
          case delegating of
            False ->
                nonDelegatingKeyMap startStr state
            True ->
                (Map.map . second . result)
                (State delegating . Widget.WidgetState child) childKeys
                `Map.union`
                delegatingKeyMap stopStr state

    size drawInfo _ (State _ widget)  =
        Widget.onWidgetState widget $ Widget.size drawInfo

    draw drawInfo (FocusDelegator focusColor _ _) (State delegating widget) = do
        let haveFocus = Widget.diHasFocus drawInfo && not delegating
            childDrawInfo = Widget.DrawInfo $
                            Widget.diHasFocus drawInfo && delegating
        if haveFocus
          then do
            size <- Draw.computeToDraw $
                    Widget.onWidgetState widget $
                    Widget.size childDrawInfo
            Draw.rect focusColor size
            return ()
          else
            return ()
        Widget.onWidgetState widget $ Widget.draw childDrawInfo
