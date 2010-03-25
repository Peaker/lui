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
    ,newDelegated)
where

import qualified Graphics.UI.LUI.Widget as Widget
import Graphics.UI.LUI.Widget(WidgetFuncs(..))

import qualified Graphics.UI.LUI.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.LUI.Keymap as Keymap
import Graphics.UI.LUI.Keymap(Keymap, ModKey)
import qualified Graphics.UI.LUI.KeyGroup as KeyGroup

import Data.Accessor(Accessor, accessor, (^.), setVal)

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Font, Color, (%%))

import qualified Data.Map as Map
import Data.Monoid(mappend, mconcat)
import Control.Category((>>>))

type Cursor = Int

defaultCursorWidth :: Draw.R
defaultCursorWidth = 0.005

data Mutable = Mutable {
  mutableText :: String,
  mutableCursor :: Cursor
  }
-- TODO: fclabels
aMutableCursor :: Accessor Mutable Cursor
aMutableCursor = accessor mutableCursor (\n x -> x{mutableCursor=n})
aMutableText :: Accessor Mutable String
aMutableText = accessor mutableText  (\n x -> x{mutableText=n})

insert :: ModKey -> Mutable -> Mutable
insert key (Mutable oldText oldCursor) = Mutable newText (oldCursor + length iText)
  where
    iText = maybe "" return . KeyGroup.unicodeOf $ key
    (preOldText, postOldText) = splitAt oldCursor oldText
    newText = concat [preOldText, iText, postOldText]

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

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

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

actBackspace = ("Delete previous character",  delBackward 1)
actDelete    = ("Delete next character",      delForward 1)
actMovePrev  = ("Move to previous character", moveCursor (subtract 1))
actMoveNext  = ("Move to next character",     moveCursor (+1))
actHome      = ("Move to beginning of text",  goHome)
actEnd       = ("Move to end of text",        goEnd)

keysMap :: Mutable -> Keymap Mutable
keysMap = insertionActions `mappend` normalActions

insertionActions :: Mutable -> Keymap Mutable
insertionActions mutable =
  Keymap.fromGroups [("Printables", ("Insert", printablesMap))]
  where
    printablesMap = Map.fromList
                    [ (key, insert key mutable)
                    | key <- KeyGroup.printables ]

normalActions :: Mutable -> Keymap Mutable
normalActions mutable@(Mutable text cursor) =
  -- Keymap (Mutable -> Mutable) -> Keymap Mutable
  fmap ($mutable) .

  mconcat . concat $ [
    [backActions | cursor > 0],
    [forwardActions | cursor < length text]
  ]
  where
    backActions =
      mconcat [
        Keymap.simpleton KeyGroup.leftKey `uncurry` actMovePrev,

        Keymap.simpleton KeyGroup.backspaceKey `uncurry` actBackspace,
        Keymap.simpleton (KeyGroup.ctrlCharKey 'h') `uncurry` actBackspace,

        Keymap.simpleton KeyGroup.homeKey `uncurry` actHome,
        Keymap.simpleton (KeyGroup.ctrlCharKey 'a') `uncurry` actHome
      ]
    forwardActions =
      mconcat [
        Keymap.simpleton KeyGroup.rightKey `uncurry` actMoveNext,

        Keymap.simpleton KeyGroup.deleteKey `uncurry` actDelete,
        Keymap.simpleton (KeyGroup.ctrlCharKey 'd') `uncurry` actDelete,

        Keymap.simpleton KeyGroup.endKey `uncurry` actEnd,
        Keymap.simpleton (KeyGroup.ctrlCharKey 'e') `uncurry` actEnd
       ]

new :: Draw.R -> Color -> Color -> Font -> Color -> Widget.New model Mutable
new cursorWidth bgColor cursorColor font textColor acc model =
  let mutable@(Mutable text cursor) = model ^. acc
      textImage = textColor `Draw.tint` Widget.drawText font text
      ts = Widget.textSize font text
  in WidgetFuncs {
    widgetImage = \drawInfo ->
      if Widget.diHasFocus drawInfo
      then
        let (w, h) = Widget.textSize font $ take cursor text
            cursorSize = (cursorWidth, h)
            cursorPos = (w, -h)
        in
          mconcat [
            (Draw.translate cursorPos `mappend` Widget.scale cursorSize) %%
            cursorColor `Draw.tint` Widget.rect,
            textImage,
            (Draw.translate (0, -h) `mappend` Widget.scale ts) %%
            bgColor `Draw.tint` Widget.rect
          ]
      else
        textImage

  , widgetSize = const ts

  , widgetGetKeymap =
    let applyToModel newMutable = acc `setVal` newMutable $ model
    in Just . fmap applyToModel . keysMap $ mutable
  }

type DelegatedMutable = FocusDelegator.DelegatedMutable Mutable

aDelegatedMutableCursor :: Accessor DelegatedMutable Cursor
aDelegatedMutableCursor = FocusDelegator.aDelegatedMutable >>> aMutableCursor
aDelegatedMutableText :: Accessor DelegatedMutable String
aDelegatedMutableText = FocusDelegator.aDelegatedMutable >>> aMutableText
delegatedMutable :: Bool -> String -> Cursor -> DelegatedMutable
delegatedMutable startInside text cursor =
    (FocusDelegator.Mutable startInside, Mutable text cursor)

newDelegatedWith :: Color -> Draw.R -> Color -> Color -> Font -> Color ->
                    Widget.New model DelegatedMutable
newDelegatedWith focusColor cursorWidth bgColor cursorColor font textColor acc =
    let textEdit = new cursorWidth bgColor cursorColor font textColor $
                   acc >>> FocusDelegator.aDelegatedMutable
    in FocusDelegator.newWith focusColor "Start editing" "Stop editing" textEdit $
       acc >>> FocusDelegator.aFocusDelegatorMutable

newDelegated :: Color -> Color -> Font -> Color ->
                Widget.New model DelegatedMutable
newDelegated = newDelegatedWith FocusDelegator.defaultFocusColor defaultCursorWidth
