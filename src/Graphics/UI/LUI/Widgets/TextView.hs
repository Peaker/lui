{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.TextView(new)
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Image as Image
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))

import Graphics.UI.HaskGame.Font(Font)
import Graphics.UI.HaskGame.Color(Color)

new :: Color -> Font -> String -> Widget model
new textColor textSize text =
    const $
    WidgetFuncs
    {
      widgetImage = const $ Image.text textColor textSize text
    , widgetSize = const $ Image.textSize textSize text
    , widgetGetKeymap = Nothing
    }
