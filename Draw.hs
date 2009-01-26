{-# OPTIONS -Wall -O2
  -XGeneralizedNewtypeDeriving
  #-}

module Draw(
           -- The monads
            Draw, Compute
           -- Monad runners
           , render, computeResult
           -- Compute primitives
           , textSize
           -- Draw primitives
           , computeToDraw
           , text, rect, move
           ) where

import Vector2(Vector2(..))
import Control.Monad.Trans(lift)
import Control.Monad.Reader(ReaderT, ask, local, runReaderT)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified MySDL

type Position = Vector2 Int
type Size = Vector2 Int

newtype Compute a = Compute { unCompute :: ReaderT TTF.Font IO a }
    deriving Monad
liftFont :: ReaderT TTF.Font IO a -> Compute a
liftFont = Compute
liftIO :: IO a -> Compute a
liftIO = Compute . lift

-- Monad Transformers require a bit of boiler-plate...
newtype Draw a = Draw { unDraw :: ReaderT SDL.Surface (ReaderT Position Compute) a }
    deriving Monad
liftPosition :: ReaderT Position Compute a -> Draw a
liftPosition = Draw . lift
liftSurface :: ReaderT SDL.Surface (ReaderT Position Compute) a -> Draw a
liftSurface = Draw

computeToDraw :: Compute a -> Draw a
computeToDraw = Draw . lift . lift

computeResult :: TTF.Font -> Compute a -> IO a
computeResult font = flip runReaderT font . unCompute

render :: TTF.Font -> SDL.Surface -> Position -> Draw a -> IO a
render font surface pos = computeResult font .
                          flip runReaderT pos .
                          flip runReaderT surface .
                          unDraw

textSize :: String -> Compute Size
textSize str = do
  font <- liftFont ask
  liftIO $ MySDL.textSize font str

text :: SDL.Color -> String -> Draw Size
text color str = do
  surface <- liftSurface ask
  position <- liftPosition ask
  computeToDraw $ do
    font <- liftFont $ ask
    textSurface <- liftIO $ MySDL.renderText font str color
    liftIO $ MySDL.blit surface position textSurface
    textSize $ str
  
rect :: SDL.Color -> Size -> Draw Size
rect color size = do
  surface <- liftSurface $ ask
  position <- liftPosition ask
  let r = MySDL.makeRect position size
  computeToDraw . liftIO $ MySDL.fillRect surface r color
  return size

move :: Position -> Draw a -> Draw a
move delta (Draw act) = do
  surface <- liftSurface $ ask
  let posReaderT = flip runReaderT surface act
  liftPosition $ local (+delta) posReaderT
