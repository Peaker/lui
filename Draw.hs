{-# OPTIONS -Wall -O2
  -XGeneralizedNewtypeDeriving
  #-}

module Draw(Draw, text, rect, move, render, textSize) where

import Vector2(Vector2(..))
import Control.Monad.Trans(MonadIO, lift, liftIO)
import Control.Monad.Reader(ReaderT, ask, local, runReaderT)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified MySDL

type Position = Vector2 Int
type Size = Vector2 Int

fRunReaderT :: r -> ReaderT r m a -> m a
fRunReaderT = flip runReaderT

-- Monad Transformers require a bit of boiler-plate...
newtype Draw a = Draw (ReaderT SDL.Surface (ReaderT Position (ReaderT TTF.Font IO)) a)
    deriving (Monad, MonadIO)
liftFont :: ReaderT TTF.Font IO a -> Draw a
liftFont = Draw . lift . lift
liftPosition :: ReaderT Position (ReaderT TTF.Font IO) a -> Draw a
liftPosition = Draw . lift
liftSurface :: ReaderT SDL.Surface
               (ReaderT Position
                (ReaderT TTF.Font IO)) a
               -> Draw a
liftSurface = Draw

render :: TTF.Font -> SDL.Surface -> Position -> Draw a -> IO a
render font surface pos (Draw act) = fRunReaderT font . fRunReaderT pos . fRunReaderT surface $ act

textSize :: String -> Draw Size
textSize str = do
  font <- liftFont ask
  liftIO $ MySDL.textSize font str

text :: SDL.Color -> String -> Draw ()
text color str = do
  font <- liftFont ask
  textSurface <- liftIO $ MySDL.renderText font str color
  surface <- liftSurface ask
  position <- liftPosition ask
  liftIO $ MySDL.blit surface position textSurface
  
rect :: MySDL.Color -> Size -> Draw ()
rect color size = do
  surface <- liftSurface $ ask
  position <- liftPosition ask
  let r = MySDL.makeRect position size
  liftIO $ MySDL.fillRect surface r color

move :: Position -> Draw a -> Draw a
move delta (Draw act) = do
  surface <- liftSurface $ ask
  let posReaderT = fRunReaderT surface act
  liftPosition $ local (+delta) posReaderT
