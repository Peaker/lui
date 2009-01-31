{-# OPTIONS_GHC -Wall -O2
 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import qualified MySDLKeys
import qualified Draw
import qualified Control.Monad.State as State

import qualified Widget
import Widget(Widget, WidgetFuncs(..))

import qualified Widgets.TextEdit as TextEdit
import qualified Widgets.FocusDelegator as FocusDelegator
import qualified Widgets.TextView as TextView
import qualified Widgets.Grid as Grid
import qualified Widgets.Box as Box
import qualified Widgets.Space as Space
import qualified Widgets.KeysTable as KeysTable
import qualified Data.Map as Map
import Vector2(Vector2(..))
import Control.Monad(forM, forM_, msum)
import Control.Monad.Trans(lift)
import Accessor(afirst, asecond, aMapValue, (^>))

import Data.Maybe(fromMaybe)

-- Commented out for 6.8's lack of the new Exception
-- import qualified Control.Exception as Exc
-- import Data.Typeable(Typeable)

speed :: Num a => a
speed = 30

-- Commented out for 6.8's lack of the new Exception
-- data QuitRequest = QuitRequest
--   deriving (Typeable, Show)
-- instance Exc.Exception QuitRequest where

handleKeyAction :: WidgetFuncs model ->
                   Widget.KeyStatus -> SDL.Keysym -> Maybe model
handleKeyAction widgetFuncs keyStatus keySym =
  let key = MySDLKey.keyOfEvent keySym
      keyGroups = MySDLKeys.groupsOfKey key
      mKeyHandler = msum $ map lookupGroup keyGroups
      lookupGroup keyGroup = Map.lookup (keyStatus, keyGroup) =<<
                             widgetGetKeymap widgetFuncs
      runHandler (_, func) = func key
  in fmap runHandler mKeyHandler

handleEvents :: [SDL.Event] -> Widget model -> State.StateT model IO Bool
handleEvents events widget =
  fmap or $ forM events $ \event -> do
    model <- State.get
    let mNewModel = case event of
                      -- 6.8 exceptions :-(
                      -- lift $ Exc.throwIO QuitRequest
                      SDL.Quit -> error "Quit"
                      SDL.KeyDown k -> handleKeyAction (widget model) Widget.KeyDown k
                      SDL.KeyUp k -> handleKeyAction (widget model) Widget.KeyUp k
                      _ -> Nothing
    case mNewModel of
      Nothing ->
          return False
      Just newState -> do
          State.put newState
          return True

mainLoop :: Widget model -> model -> IO ()
mainLoop widget initModel = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display $ SDL.Color 0 0 0
  (`State.evalStateT` initModel) $
    forM_ (True:repeat False) $ \shouldDraw -> do
      events <- lift $ MySDL.getEvents
      handledEvent <- handleEvents events widget
      model <- State.get
      lift $ do
        SDL.fillRect display Nothing blackPixel
        if handledEvent || shouldDraw
          then do
            -- forM_ (Map.assocs $ fromMaybe Map.empty $ Widget.getKeymap widget model) $
            --   \((_, group), (desc, _)) -> do
            --     print (MySDLKey.keyGroupName group, desc)
            let draw = widgetDraw (widget model) (Widget.DrawInfo True)
            Draw.render display (Vector2 0 0) draw
            SDL.flip display
          else
            SDL.delay 20

main :: IO ()
main =
  MySDL.withSDL $ do
    font <- MySDL.defaultFont 30
    keysFont <- MySDL.defaultFont 20
    descFont <- MySDL.defaultFont 15
    let textEditingColor = SDL.Color 30 20 100
        textEditColor = SDL.Color 255 255 255
        textViewColor = SDL.Color 255 100 255
        textEditCursorColor = SDL.Color 255 0 0
        textEditCursorWidth = 2
        keysColor = SDL.Color 255 0 0
        descColor = SDL.Color 0 0 255
        focusColor = SDL.Color 0 0 150

        startOutside = FocusDelegator.Mutable False
        dTextEditModel = (startOutside, TextEdit.Mutable "Hello world" 5)
        dTextEditModels = Map.fromList [((x, y), dTextEditModel)
                                        | x <- [0..1]
                                       , y <- [0..1]]
        dGridModel = (startOutside, Grid.Mutable (0, 0))
        boxModel = Box.Mutable 0
        dBoxModel = (startOutside, boxModel)
        initModel = ((boxModel, dBoxModel),(dTextEditModels,dGridModel))

        textEdit cursor =
            TextEdit.newDelegated
            (const $
             (focusColor,
              TextEdit.Immutable
                      textEditingColor
                      textEditCursorColor
                      textEditCursorWidth
                      font
                      textEditColor)) $
            asecond ^> afirst ^> aMapValue cursor
        grid = Grid.newDelegated
               (const $
                (focusColor,
                 Grid.Immutable
                         (2, 2)
                         items)) $
               asecond ^> asecond
        items = Map.fromList
                [((x, y), Grid.Item (textEdit (x, y)) (0.5, 1))
                 | x <- [0..1], y <- [0..1]]

        textView = TextView.new
                   (const $
                    TextView.Immutable
                            textViewColor
                            font
                            "This is just a view")
        vbox = Box.newDelegated
               (const $
                (focusColor,
                 Box.Immutable
                    Box.Vertical
                    vboxItems)) $
               afirst ^> asecond
        space = Space.new
                (const . Space.Immutable $ Vector2 50 50)
        vboxItems = [Box.Item grid 1
                    ,Box.Item space 0.5
                    ,Box.Item (textEdit (0, 1)) 0.5
                    ,Box.Item textView 0.5]
        keysTable = KeysTable.new
                    (let handlers = fromMaybe Map.empty . widgetGetKeymap . hbox
                     in KeysTable.Immutable keysColor keysFont
                                            descColor descFont
                        . handlers)
        hbox = Box.new
               (const $ Box.Immutable Box.Horizontal hboxItems) $
               afirst ^> afirst
        hboxItems = [Box.Item vbox 0.5
                    ,Box.Item keysTable 0]

        runWidget = mainLoop hbox initModel

    -- Commented out for 6.8's lack of the new Exception
    -- flip Exc.catch errHandler runWidget
    -- where
    --   errHandler :: QuitRequest -> IO ()
    --   errHandler = const . putStrLn $ "Quit requested"
    runWidget
