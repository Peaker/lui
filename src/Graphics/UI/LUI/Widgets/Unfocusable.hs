{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Unfocusable(new)
where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

new :: Widget model -> Widget model
new childWidget model =
    let childWidgetFuncs = childWidget model
    in WidgetFuncs
    {
      widgetSize = \_ -> widgetSize childWidgetFuncs noFocusDrawInfo
    , widgetImage = \_ -> widgetImage childWidgetFuncs noFocusDrawInfo
    , widgetGetKeymap = Nothing
    }
