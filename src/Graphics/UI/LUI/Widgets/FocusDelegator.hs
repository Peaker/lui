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
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import Graphics.UI.LUI.KeyGroup(KeyGroupName, returnKey, escapeKey)

import Graphics.UI.LUI.Keymap(Keymap, ModKey, Doc)
import qualified Graphics.UI.LUI.Keymap as Keymap

import Data.Accessor(Accessor, (^.), setVal)
import qualified Data.Accessor.Tuple
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))
import Data.Maybe(fromMaybe)
import Data.Monoid(Monoid(..))

-- TODO: Use record instead of tuple so auto-TH creates the accessors:
type DelegatedMutable mutable = (Mutable, mutable)
aDelegatedMutable :: Accessor (DelegatedMutable mutable) mutable
aDelegatedMutable = Data.Accessor.Tuple.second
aFocusDelegatorMutable :: Accessor (DelegatedMutable mutable) Mutable
aFocusDelegatorMutable = Data.Accessor.Tuple.first

-- defaults:
defaultFocusColor :: Draw.Color
defaultFocusColor = Draw.Color 0 0 0.7 1

data Mutable = Mutable
    {
      mutableDelegateFocus :: Bool
    }

mkKeymap :: KeyGroupName -> Doc -> ModKey -> Bool -> Keymap Mutable
mkKeymap gn doc key b = Keymap.singleton gn doc key (Mutable b)

nonDelegatingKeymap :: Doc -> Keymap Mutable
nonDelegatingKeymap doc = mkKeymap "Return" doc returnKey True

delegatingKeymap :: Doc -> Keymap Mutable
delegatingKeymap doc = mkKeymap "Escape" doc escapeKey False

newWith :: Draw.Color -> String -> String -> Widget model -> Widget.New model Mutable
newWith focusColor startStr stopStr childWidget acc model =
    let Mutable delegating = model ^. acc
        childWidgetFuncs = childWidget model
        childWidgetSize = widgetSize childWidgetFuncs
    in WidgetFuncs
    {
      widgetSize = childWidgetSize
    , widgetImage = \drawInfo ->
        let haveFocus = Widget.diHasFocus drawInfo
            delegatorHasFocus = haveFocus && not delegating
            childDrawInfo = Widget.DrawInfo { Widget.diHasFocus = haveFocus && delegating }
            size@(_, childHeight) = childWidgetSize childDrawInfo
        in widgetImage childWidgetFuncs childDrawInfo
           `mappend`
           if delegatorHasFocus
           then Draw.translate (0, -childHeight) `mappend`
                Widget.scale size %%
                focusColor `Draw.tint` Widget.rect
           else mempty
           
    , widgetGetKeymap =
        let mChildKeymap = widgetGetKeymap childWidgetFuncs
            childKeymap = fromMaybe mempty mChildKeymap
            applyToModel newMutable = acc `setVal` newMutable $ model
            wrapKeymap = fmap applyToModel
        in case delegating of
             True ->
                 Just $
                 childKeymap `mappend` (wrapKeymap $ delegatingKeymap stopStr)
             False ->
                 const (wrapKeymap $ nonDelegatingKeymap startStr)
                 -- Only expose the nonDelegatingKeymap if the child
                 -- has a keymap:
                 `fmap` mChildKeymap
    }

new :: String -> String -> Widget model -> Widget.New model Mutable
new = newWith defaultFocusColor
