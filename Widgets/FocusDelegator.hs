{-# OPTIONS_GHC -Wall -O2
    -XMultiParamTypeClasses
  #-}

module Widgets.FocusDelegator where

import qualified Widget
import Widget(Widget(..))
import MySDLKey(asKeyGroup, noMods)
import qualified MySDLKey
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Control.Arrow(second)
import Func(result)
import Accessor((^.), (^:))
import Data.Maybe(fromMaybe)

data Mutable = Mutable {
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

type New model mutable = String -> String -> SDL.Color ->
                         Widget model -> Widget.New model mutable

new :: New model Mutable
new startStr stopStr focusColor childWidget accessor =
    Widget
    {
      widgetSize = \drawInfo model -> widgetSize childWidget drawInfo model
    , widgetDraw = \drawInfo model -> do
        let Mutable delegating = model ^. accessor
            haveFocus = Widget.diHasFocus drawInfo
            delegatorHasFocus = haveFocus && not delegating
            childDrawInfo = Widget.DrawInfo
                            {
                              Widget.diHasFocus = haveFocus && delegating
                            }
        if delegatorHasFocus
          then do
            size <- Draw.computeToDraw $
                    widgetSize childWidget childDrawInfo model
            Draw.rect focusColor size
            return ()
          else
            return ()
        widgetDraw childWidget childDrawInfo model
    , widgetGetKeymap = \model ->
        let Mutable delegating = model ^. accessor
            mChildKeymap = widgetGetKeymap childWidget model
            childKeymap = fromMaybe Map.empty mChildKeymap
            onModel newMutable = (accessor ^: (const newMutable)) model
            wrapKeymap = (Map.map . second . result) onModel
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
