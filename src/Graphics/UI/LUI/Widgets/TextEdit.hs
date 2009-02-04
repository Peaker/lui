{-# OPTIONS_GHC -Wall -O2
  #-}

module Graphics.UI.LUI.Widgets.TextEdit
    (Mutable(..)
    ,aMutableCursor
    ,aMutableText
    ,Cursor
    ,defaultCursorWidth
    ,new
    ,DelegatedMutable
    ,aDelegatedMutableCursor
    ,aDelegatedMutableText
    ,delegatedMutable
    ,newDelegatedWith
    ,newDelegated
    )
where

import qualified Graphics.UI.LUI.Widget as Widget
import qualified Graphics.UI.LUI.Image as Image
import qualified Graphics.UI.LUI.Widgets.FocusDelegator as FocusDelegator
import Graphics.UI.LUI.Widget(WidgetFuncs(..))

import Graphics.UI.LUI.Func(result)
import Graphics.UI.LUI.List(isSorted)
import Graphics.UI.LUI.Accessor(Accessor, accessor, (^.), (^>), write)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.HaskGame.Key as Key
import qualified Graphics.UI.HaskGame.Keys as Keys
import Graphics.UI.HaskGame.Key(asKeyGroup, noMods, ctrl)
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Graphics.UI.HaskGame.Color(Color)
import Graphics.UI.HaskGame.Font(Font)

import qualified Data.Map as Map
import Data.Map((!))
import Data.Monoid(mconcat)
import Control.Arrow(first, second)

type Cursor = Int

defaultCursorWidth :: Int
defaultCursorWidth = 2

data Mutable = Mutable
    {
      mutableText :: String
    , mutableCursor :: Cursor
    }
-- TODO: TH
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = accessor mutableCursor (\n x -> x{mutableCursor=n})
aMutableText :: Accessor Mutable String
aMutableText = accessor mutableText  (\n x -> x{mutableText=n})

insert :: Mutable -> Key.ModKey -> Mutable
insert (Mutable oldText oldCursor) key =
    let iText = Keys.keysUnicode!key
        (preOldText, postOldText) = splitAt oldCursor oldText
        newText = concat [preOldText, iText, postOldText]
        newCursor = oldCursor + length iText
    in Mutable newText newCursor

delBackward :: Int -> Mutable -> Mutable
delBackward count (Mutable oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPreText = take (length oldPreText - count) oldPreText
        newText = newPreText ++ oldPostText
        newCursor = length newPreText
    in Mutable newText newCursor

delForward :: Int -> Mutable -> Mutable
delForward count (Mutable oldText oldCursor) =
    let (oldPreText, oldPostText) = splitAt oldCursor oldText
        newPostText = drop count oldPostText
        newText = oldPreText ++ newPostText
    in Mutable newText oldCursor

moveCursor :: (Cursor -> Cursor) -> Mutable -> Mutable
moveCursor cursorFunc (Mutable text oldCursor) =
    let newCursor = cursorFunc oldCursor
    in Mutable text $ if isSorted [0, newCursor, length text]
                            then newCursor
                            else oldCursor

goHome :: Mutable -> Mutable
goHome (Mutable text _) = Mutable text 0

goEnd :: Mutable -> Mutable
goEnd (Mutable text _) = Mutable text (length text)

actBackspace, actDelete, actMovePrev, actMoveNext, actHome, actEnd ::
    (String, Mutable -> Mutable)

actBackspace = ("Delete previous character", delBackward 1)
actDelete = ("Delete next character",        delForward 1)
actMovePrev = ("Move to previous character", moveCursor (subtract 1))
actMoveNext = ("Move to next character",     moveCursor (+1))
actHome = ("Move to beginning of text",      goHome)
actEnd = ("Move to end of text",             goEnd)

keysMap :: Mutable -> Widget.ActionHandlers Mutable
keysMap mutable = Map.fromList . (map . first) ((,) Widget.KeyDown) $
    (Keys.printableGroup, ("Insert", insert mutable)) :
    (map . second . second) (const . ($mutable)) (normalActions mutable ++ ctrlActions mutable)

cond :: Bool -> [a] -> [a]
cond p i = if p then i else []

normalActions :: Mutable -> [(Key.KeyGroup, (String, Mutable -> Mutable))]
normalActions mutable =
    let cursor = mutableCursor mutable
        text = mutableText mutable
    in (map . first) (asKeyGroup noMods) . concat $
           [cond (cursor > 0)
                     [(SDL.SDLK_BACKSPACE, actBackspace)
                     ,(SDL.SDLK_LEFT, actMovePrev)
                     ,(SDL.SDLK_HOME, actHome)]
           ,cond (cursor < length text)
                     [(SDL.SDLK_DELETE, actDelete)
                     ,(SDL.SDLK_RIGHT, actMoveNext)
                     ,(SDL.SDLK_END, actEnd)]
           ]

ctrlActions :: Mutable -> [(Key.KeyGroup, (String, Mutable -> Mutable))]
ctrlActions mutable =
    let cursor = mutableCursor mutable
        text = mutableText mutable
    in (map . first) (asKeyGroup ctrl) . concat $
           [cond (cursor > 0)
                     [(SDL.SDLK_h, actBackspace)
                     ,(SDL.SDLK_a, actHome)]
           ,cond (cursor < length text)
                     [(SDL.SDLK_d, actDelete)
                     ,(SDL.SDLK_e, actEnd)]
           ]

new :: Int -> Color -> Color -> Font -> Color -> Widget.New model Mutable
new cursorWidth bgColor cursorColor font textColor acc model =
  let mutable@(Mutable text cursor) = model ^. acc
  in WidgetFuncs
  {
    widgetImage = \drawInfo ->
      if Widget.diHasFocus drawInfo
      then
        let textSize = Image.textSize font text
            Vector2 w h = Image.textSize font $ take cursor text
            cursorSize = Vector2 cursorWidth h
            cursorPos = Vector2 w 0
        in
          mconcat
          [
           Image.rect bgColor textSize
          ,Image.text textColor font text
          ,Image.move cursorPos $ Image.rect cursorColor cursorSize
          ]
      else
        Image.text textColor font text

  , widgetSize = const $ Image.textSize font text

  , widgetGetKeymap =
    let applyToModel newMutable = acc `write` newMutable $ model
    in Just $
       (Map.map . second . result) applyToModel $ keysMap mutable
  }

type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable ^> aMutableCursor
aDelegatedMutableText :: Accessor DelegatedMutable String
aDelegatedMutableText = FocusDelegator.aDelegatedMutable ^> aMutableText
delegatedMutable :: Bool -> String -> Cursor -> DelegatedMutable
delegatedMutable startInside text cursor =
    (FocusDelegator.Mutable startInside, Mutable text cursor)

newDelegatedWith :: Color -> Int -> Color -> Color -> Font -> Color ->
                    Widget.New model DelegatedMutable
newDelegatedWith focusColor cursorWidth bgColor cursorColor font textColor acc =
    let textEdit = new cursorWidth bgColor cursorColor font textColor $
                   acc ^> FocusDelegator.aDelegatedMutable
    in FocusDelegator.newWith focusColor "Start editing" "Stop editing" textEdit $
       acc ^> FocusDelegator.aFocusDelegatorMutable

newDelegated :: Color -> Color -> Font -> Color ->
                Widget.New model DelegatedMutable
newDelegated = newDelegatedWith FocusDelegator.defaultFocusColor defaultCursorWidth
