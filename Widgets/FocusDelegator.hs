{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.FocusDelegator where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))
import HaskGame.Key(asKeyGroup, noMods)
import qualified HaskGame.Key as Key
import qualified HaskGame.Color as Color
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Control.Arrow(second)
import Func(result)
import Accessor((^.), write)
import Data.Maybe(fromMaybe)

data Immutable model = Immutable
    {
      immutableStartStr :: String
    , immutableStopStr :: String
    , immutableChildWidget :: Widget model
    , immutableFocusColor :: Color.Color
    }

data Mutable = Mutable
    {
      mutableDelegateFocus :: Bool
    }

buildKeymap :: SDL.SDLKey -> String -> Bool -> Widget.ActionHandlers Mutable
buildKeymap key desc newDelegating =
    Map.singleton (Widget.KeyDown, asKeyGroup noMods key)
                  (desc, const $ Mutable newDelegating)

delegatingKeyMap, nonDelegatingKeyMap ::
    String -> Widget.ActionHandlers Mutable
nonDelegatingKeyMap startStr = buildKeymap SDL.SDLK_RETURN startStr True
delegatingKeyMap    stopStr  = buildKeymap SDL.SDLK_ESCAPE stopStr False

new :: Widget.New model (Immutable model) Mutable
new immutableMaker acc model =
    let Immutable startStr stopStr childWidget focusColor = immutableMaker model
        Mutable delegating = model ^. acc
        childWidgetFuncs = childWidget model
    in WidgetFuncs
    {
      widgetSize = \drawInfo -> widgetSize childWidgetFuncs drawInfo
    , widgetDraw = \drawInfo -> do
        let 
            haveFocus = Widget.diHasFocus drawInfo
            delegatorHasFocus = haveFocus && not delegating
            childDrawInfo = Widget.DrawInfo
                            {
                              Widget.diHasFocus = haveFocus && delegating
                            }
        if delegatorHasFocus
          then do
            size <- Draw.computeToDraw $
                    widgetSize childWidgetFuncs childDrawInfo
            Draw.rect focusColor size
            return ()
          else
            return ()
        widgetDraw childWidgetFuncs childDrawInfo
    , widgetGetKeymap =
        let mChildKeymap = widgetGetKeymap childWidgetFuncs
            childKeymap = fromMaybe Map.empty mChildKeymap
            applyToModel newMutable = acc `write` newMutable $ model
            wrapKeymap = (Map.map . second . result) applyToModel
        in case delegating of
             True ->
                 Just $
                 childKeymap `Map.union` (wrapKeymap $ delegatingKeyMap stopStr)
             False ->
                 const (wrapKeymap $ nonDelegatingKeyMap startStr)
                 -- Only expose the nonDelegatingKeyMap if the child
                 -- has a keymap:
                 `fmap` mChildKeymap
    }
