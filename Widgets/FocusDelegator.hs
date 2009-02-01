{-# OPTIONS_GHC -Wall -O2
  #-}

module Widgets.FocusDelegator where

import qualified Widget
import Widget(Widget, WidgetFuncs(..))
import HaskGame.Key(asKeyGroup, noMods)
import qualified HaskGame.Key as Key
import HaskGame.Color(Color(..))
import qualified Graphics.UI.SDL as SDL
import qualified Draw
import qualified Data.Map as Map
import Control.Arrow(second)
import Func(result)
import Accessor(Accessor, afirst, asecond, (^.), write)
import Data.Maybe(fromMaybe)

-- TODO: Use record instead of tuple so auto-TH creates the accessors:
type DelegatedMutable mutable = (Mutable, mutable)
aDelegatedMutable :: Accessor (DelegatedMutable mutable) mutable
aDelegatedMutable = asecond
aFocusDelegatorMutable :: Accessor (DelegatedMutable mutable) Mutable
aFocusDelegatorMutable = afirst

-- defaults:
defaultFocusColor :: Color
defaultFocusColor = Color 0 0 150

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

newWith :: Color -> String -> String -> Widget model -> Widget.New model Mutable
newWith focusColor startStr stopStr childWidget acc model =
    let Mutable delegating = model ^. acc
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

new :: String -> String -> Widget model -> Widget.New model Mutable
new = newWith defaultFocusColor
