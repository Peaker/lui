{-# OPTIONS -Wall -O2
  -XGeneralizedNewtypeDeriving
  #-}

module Draw(Position,Size,Font
           -- The monads
           ,Draw
           ,Compute
           -- Monad runners
           ,render
           ,computeResult
           -- Compute primitives
           ,textSize
           -- Draw primitives
           ,computeToDraw
           ,text
           ,rect
           ,move
           ) where

import Vector2(Vector2(..))
import Control.Monad.Trans(lift)
import Control.Monad.Reader(ReaderT, ask, local, runReaderT)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified MySDL

type Position = Vector2 Int
type Size = Vector2 Int
type Font = TTF.Font

newtype Compute a = Compute { unCompute :: IO a }
    deriving Monad
liftIO :: IO a -> Compute a
liftIO = Compute

-- Monad Transformers require a bit of boiler-plate...
newtype Draw a = Draw { unDraw :: ReaderT SDL.Surface (ReaderT Position Compute) a }
    deriving Monad
liftPosition :: ReaderT Position Compute a -> Draw a
liftPosition = Draw . lift
liftSurface :: ReaderT SDL.Surface (ReaderT Position Compute) a -> Draw a
liftSurface = Draw

computeToDraw :: Compute a -> Draw a
computeToDraw = Draw . lift . lift

computeResult :: Compute a -> IO a
computeResult = unCompute

render :: SDL.Surface -> Position -> Draw a -> IO a
render surface pos = computeResult .
                     flip runReaderT pos .
                     flip runReaderT surface .
                     unDraw

textSize :: Font -> String -> Compute Size
textSize font str = do
  liftIO $ MySDL.textSize font str

text :: SDL.Color -> Font -> String -> Draw Size
text color font str = do
  surface <- liftSurface ask
  position <- liftPosition ask
  computeToDraw $ do
    textSurface <- liftIO $ MySDL.renderText font str color
    liftIO $ MySDL.blit surface position textSurface
    textSize font str
  
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
