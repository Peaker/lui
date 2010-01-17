{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.Scroll
    (new
    ,Mutable(..))
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Image as Image
import Graphics.UI.LUI.Widget(Widget, WidgetFuncs(..))
import Graphics.UI.HaskGame.Key(asKeyGroup, noMods)
import Graphics.UI.LUI.Accessor(Accessor, (^.), write)

import Graphics.UI.HaskGame.Vector2(Vector2(..))

import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map
import Data.Monoid(Monoid(..))
import Control.Applicative(liftA2)

data Mutable = Mutable
    {
      mutablePos :: Vector2 Int
    }

leftKeyGroup,
 rightKeyGroup,
 upKeyGroup,
 downKeyGroup :: Widget.KeyAction

leftKeyGroup  = (Widget.KeyDown, asKeyGroup noMods SDL.SDLK_LEFT)
rightKeyGroup = (Widget.KeyDown, asKeyGroup noMods SDL.SDLK_RIGHT)
upKeyGroup    = (Widget.KeyDown, asKeyGroup noMods SDL.SDLK_UP)
downKeyGroup  = (Widget.KeyDown, asKeyGroup noMods SDL.SDLK_DOWN)

hjump :: Int
hjump = 10

vjump :: Int
vjump = 10

makeKeymap :: Vector2 Int -> Vector2 Int ->
              model -> Accessor model Mutable -> Maybe (Widget.ActionHandlers model)
makeKeymap minScroll maxScroll model acc =
    let Mutable pos = model ^. acc
        clip = liftA2 max minScroll . liftA2 min maxScroll
        doesMove delta = pos /= clip (delta + pos)
        newModel delta = write acc (Mutable . clip $ delta + pos) model
        handlers =
            concat
            [if doesMove delta then
                 [(keyGroup, (desc, const $ newModel delta))]
             else
                 []
            | (keyGroup, delta, desc) <-
                [(leftKeyGroup,  Vector2 (-hjump) 0, "Scroll left")
                ,(rightKeyGroup, Vector2 vjump 0   , "Scroll right")
                ,(upKeyGroup,    Vector2 0 (-hjump), "Scroll up")
                ,(downKeyGroup,  Vector2 0 vjump   , "Scroll down")
                ]
           ]
    in if null handlers then
           Nothing
       else
           Just . Map.fromList $ handlers

-- TODO: REMOVE THIS!
noFocusDrawInfo :: Widget.DrawInfo
noFocusDrawInfo = Widget.DrawInfo False

new :: Vector2 Int -> Widget model -> Widget.New model Mutable
new size widget acc model =
    let childFuncs = widget model
    in WidgetFuncs
    {
      widgetGetKeymap = widgetGetKeymap childFuncs `mappend`
                        makeKeymap (Vector2 0 0)
                                   (widgetSize childFuncs noFocusDrawInfo - size)
                        model acc
    , widgetImage = \drawInfo ->
                    Image.crop size .
                    Image.move (negate . mutablePos $ model ^. acc) $
                    widgetImage childFuncs drawInfo
    , widgetSize = const size
    }
