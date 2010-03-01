{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.FocusDelegator
    (Mutable(..)
    ,DelegatedMutable
    ,aDelegatedMutable
    ,aFocusDelegatorMutable
    ,defaultFocusColor
    ,newWith
    ,new
    )
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Image as Image
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import Data.Editor.Function(result)
import Data.Accessor(Accessor, (^.), setVal)
import qualified Data.Accessor.Tuple

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.HaskGame.Key(asKeyGroup, noMods)
import Graphics.UI.HaskGame.Color(Color(..))

import qualified Data.Map as Map
import Control.Arrow(second)
import Data.Maybe(fromMaybe)
import Data.Monoid(Monoid(..))

-- TODO: Use record instead of tuple so auto-TH creates the accessors:
type DelegatedMutable mutable = (Mutable, mutable)
aDelegatedMutable :: Accessor (DelegatedMutable mutable) mutable
aDelegatedMutable = Data.Accessor.Tuple.second
aFocusDelegatorMutable :: Accessor (DelegatedMutable mutable) Mutable
aFocusDelegatorMutable = Data.Accessor.Tuple.first

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
        childWidgetSize = widgetSize childWidgetFuncs
    in WidgetFuncs
    {
      widgetSize = childWidgetSize
    , widgetImage = \drawInfo ->
        let 
            haveFocus = Widget.diHasFocus drawInfo
            delegatorHasFocus = haveFocus && not delegating
            childDrawInfo = Widget.DrawInfo
                            {
                              Widget.diHasFocus = haveFocus && delegating
                            }
        in (if delegatorHasFocus
            then Image.rect focusColor $ childWidgetSize childDrawInfo
            else mempty)
           `mappend`
           widgetImage childWidgetFuncs childDrawInfo
    , widgetGetKeymap =
        let mChildKeymap = widgetGetKeymap childWidgetFuncs
            childKeymap = fromMaybe Map.empty mChildKeymap
            applyToModel newMutable = acc `setVal` newMutable $ model
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
