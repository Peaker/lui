{-# OPTIONS_GHC -Wall -O2
 -XDeriveDataTypeable
 #-}

module Main where

import qualified Graphics.UI.SDL as SDL
import qualified MySDL
import qualified MySDLKey
import qualified MySDLKeys
import qualified Draw
import qualified Control.Monad.State as State

import qualified Widget
import Widget(Widget(..))

import qualified Widgets.TextEdit as TextEdit
-- import qualified Widgets.TextView as TextView
import qualified Widgets.Grid as Grid
-- import qualified Widgets.Box as Box
-- import qualified Widgets.Unfocusable as Unfocusable
-- import qualified Widgets.Space as Space
-- import qualified Widgets.KeysTable as KeysTable
import qualified Data.Map as Map
-- import Vector2(Vector2(..))
import Control.Monad(forM, forM_, msum)
import Control.Monad.Trans(lift)
import Accessor(afirst, asecond, aMapValue, (^>))
--import Data.Maybe(fromMaybe)

-- Commented out for 6.8's lack of the new Exception
-- import qualified Control.Exception as Exc
-- import Data.Typeable(Typeable)

speed :: Num a => a
speed = 30

-- Commented out for 6.8's lack of the new Exception
-- data QuitRequest = QuitRequest
--   deriving (Typeable, Show)
-- instance Exc.Exception QuitRequest where

handleKeyAction :: Widget model ->
                   Widget.KeyStatus -> SDL.Keysym -> model -> Maybe model
handleKeyAction widget keyStatus keySym model =
  let key = MySDLKey.keyOfEvent keySym
      keyGroups = MySDLKeys.groupsOfKey key
      mKeyHandler = msum $ map lookupGroup keyGroups
      lookupGroup keyGroup = Map.lookup (keyStatus, keyGroup) =<<
                             widgetGetKeymap widget model
      runHandler (_, func) = func key
  in fmap runHandler mKeyHandler

maybeModify :: State.MonadState s m => (s -> Maybe s) -> m Bool
maybeModify f = do
  state <- State.get
  maybe (return False) updateState (f state)
  where
    updateState s = do
      State.put s
      return True

handleEvents :: Widget model -> [SDL.Event] -> State.StateT model IO Bool
handleEvents widget events =
  fmap or $ forM events $ \event ->
      case event of
        SDL.Quit -> error "Quit" -- 6.8 exceptions :-(  lift $ Exc.throwIO QuitRequest
        SDL.KeyDown k -> maybeModify $
                         handleKeyAction widget Widget.KeyDown k
        SDL.KeyUp k -> maybeModify $
                       handleKeyAction widget Widget.KeyUp k
        _ -> return False

mainLoop :: Widget model -> model -> IO ()
mainLoop widget initModel = do
  display <- SDL.setVideoMode 800 600 16 [SDL.DoubleBuf]
  blackPixel <- MySDL.sdlPixel display $ SDL.Color 0 0 0
  font <- MySDL.defaultFont 30
  (`State.evalStateT` initModel) $
    forM_ (True:repeat False) $ \shouldDraw -> do
      events <- lift $ MySDL.getEvents
      handledEvent <- handleEvents widget events
      state <- State.get
      lift $ do
        SDL.fillRect display Nothing blackPixel
        if handledEvent || shouldDraw
          then do
            -- forM_ (Map.assocs $ fromMaybe Map.empty $ Widget.getKeymap widget state) $
            --   \((_, group), (desc, _)) -> do
            --     print (MySDLKey.keyGroupName group, desc)
            let draw = Widget.widgetDraw widget (Widget.DrawInfo True) state
            Draw.render font display (fromInteger 0) draw
            SDL.flip display
          else
            SDL.delay 20

main :: IO ()
main = do
  MySDL.withSDL $ do
    let textEditingColor = SDL.Color 30 20 100
        textEditColor = SDL.Color 255 255 255
        -- textViewColor = SDL.Color 255 100 255
        textEditCursorColor = SDL.Color 255 0 0
        textEditCursorWidth = 2
        -- keysColor = SDL.Color 255 0 0
        -- descColor = SDL.Color 0 0 255
        -- focusColor = SDL.Color 0 0 150
        model = (Map.fromList [((x, y), textEditModel)
                               | x <- [0..1]
                              , y <- [0..1]]
                ,gridModel)

        textEditModel = TextEdit.Mutable "Hello world" 5
        textEdit cursor =
            TextEdit.new
                    textEditingColor
                    textEditCursorColor
                    textEditCursorWidth
                    textEditColor
                    (afirst ^> aMapValue cursor)
        gridModel = Grid.Mutable (0, 0)
        grid = Grid.new (2, 2) items asecond
        items = Map.fromList
                [((x, y),
                  Grid.Item (0.5, 1) $
                  textEdit (x, y))
                 | x <- [0..1]
                , y <- [0..1]]

        -- textView = TextView.new textViewColor "This is just a view"
        -- vbox = Box.new Box.Vertical 0
        --        [Box.Item 1   . Widget.upCast $ grid
        --        ,Box.Item 0.5 . Widget.upCast $ (Space.new (Vector2 50 50))
        --        ,Box.Item 0.5 . Widget.upCast $ textEdit
        --        ,Box.Item 0.5 . Widget.upCast $ textView]
        -- keysTable = KeysTable.grid keysColor descColor . Widget.upCast $ widget
        -- hbox = Box.new Box.Horizontal 0
        --        [Box.Item 0.5 . Widget.upCast $ vbox
        --        ,Box.Item 0.1 . Widget.upCast . Unfocusable.new $ keysTable]

    let runWidget = mainLoop grid model
        --runWidget = mainLoop . Widget.upCast $ hbox

    -- Commented out for 6.8's lack of the new Exception
    -- flip Exc.catch errHandler runWidget
    -- where
    --   errHandler :: QuitRequest -> IO ()
    --   errHandler = const . putStrLn $ "Quit requested"
    runWidget
