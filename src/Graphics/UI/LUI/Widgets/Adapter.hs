{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Adapter where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import Graphics.UI.LUI.Func(result)
import Graphics.UI.LUI.Accessor(Accessor, write, (^.))

import qualified Data.Map as Map
import Control.Arrow(second)

adapt :: Accessor whole part -> Widget part -> Widget whole
adapt acc widget model =
    let widgetFuncs = widget (model ^. acc)
    in
      WidgetFuncs
      {
        widgetDraw = widgetDraw widgetFuncs
      , widgetSize = widgetSize widgetFuncs
      , widgetGetKeymap =
          let keymap = widgetGetKeymap widgetFuncs
              convertModel part = write acc part model
          in (fmap . Map.map . second . result) convertModel $ keymap
      }
