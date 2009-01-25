{-# OPTIONS -Wall -O2
  -XGeneralizedNewtypeDeriving
  #-}

module Draw(
           -- The monads
            Draw, Compute
           -- Compute primitives
           , textSize
           -- Draw primitives
           , computeToDraw
           , text, rect, move
           -- Monad runners
           , render, computeResult) where

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

newtype Compute a = Compute { unCompute :: ReaderT TTF.Font IO a }
    deriving (Monad, MonadIO)
liftFont :: ReaderT TTF.Font IO a -> Compute a
liftFont = Compute

-- Monad Transformers require a bit of boiler-plate...
newtype Draw a = Draw { unDraw :: ReaderT SDL.Surface (ReaderT Position Compute) a }
    deriving (Monad, MonadIO)
liftPosition :: ReaderT Position Compute a -> Draw a
liftPosition = Draw . lift
liftSurface :: ReaderT SDL.Surface (ReaderT Position Compute) a -> Draw a
liftSurface = Draw

computeToDraw :: Compute a -> Draw a
computeToDraw = Draw . lift . lift

computeResult :: TTF.Font -> Compute a -> IO a
computeResult font = fRunReaderT font . unCompute

render :: TTF.Font -> SDL.Surface -> Position -> Draw a -> IO a
render font surface pos = computeResult font .
                          fRunReaderT pos .
                          fRunReaderT surface .
                          unDraw

textSize :: String -> Compute Size
textSize str = do
  font <- liftFont ask
  liftIO $ MySDL.textSize font str

text :: SDL.Color -> String -> Draw Size
text color str = do
  font <- computeToDraw . liftFont $ ask
  textSurface <- liftIO $ MySDL.renderText font str color
  surface <- liftSurface ask
  position <- liftPosition ask
  liftIO $ MySDL.blit surface position textSurface
  computeToDraw . textSize $ str
  
rect :: SDL.Color -> Size -> Draw Size
rect color size = do
  surface <- liftSurface $ ask
  position <- liftPosition ask
  let r = MySDL.makeRect position size
  liftIO $ MySDL.fillRect surface r color
  return size

move :: Position -> Draw a -> Draw a
move delta (Draw act) = do
  surface <- liftSurface $ ask
  let posReaderT = fRunReaderT surface act
  liftPosition $ local (+delta) posReaderT
