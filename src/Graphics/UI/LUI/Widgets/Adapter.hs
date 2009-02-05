{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Adapter
    (adaptAll
    ,adaptModel
    ,adaptImage
    ,adaptSize)
where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import Graphics.UI.LUI.Image(Image)

import Graphics.UI.LUI.Func(result)
import Graphics.UI.LUI.Accessor(Accessor, write, (^.))

import Graphics.UI.HaskGame.Vector2(Vector2)

import qualified Data.Map as Map
import Control.Arrow(second)

type Endo a = a -> a

adaptAll :: Endo Image ->
            Endo (Vector2 Int) ->
            (Maybe (Widget.ActionHandlers part) ->
             Maybe (Widget.ActionHandlers whole)) ->
            WidgetFuncs part -> WidgetFuncs whole
adaptAll imageFunc sizeFunc keymapFunc widgetFuncs =
    WidgetFuncs
    {
      widgetImage = result imageFunc . widgetImage $ widgetFuncs
    , widgetSize = result sizeFunc . widgetSize $ widgetFuncs
    , widgetGetKeymap = keymapFunc . widgetGetKeymap $ widgetFuncs
    }

adaptModel :: Accessor whole part -> Widget part -> Widget whole
adaptModel acc widget model = adaptAll id id convertKeymap (widget (model ^. acc))
    where convertKeymap = fmap . Map.map . second . result $ convertModel
          convertModel part = write acc part model

adaptImage :: Endo Image -> Endo (Widget model)
adaptImage imageFunc widget model = adaptAll imageFunc id id $ widget model

adaptSize :: Endo (Vector2 Int) -> Endo (Widget model)
adaptSize sizeFunc widget model = adaptAll id sizeFunc id $ widget model
