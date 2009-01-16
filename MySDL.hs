{-# OPTIONS -Wall -O2 #-}

module MySDL where
    import Data.Word(Word8)
    import qualified MyMonad
    import qualified Graphics.UI.SDL as SDL
    import qualified Graphics.UI.SDL.TTF as TTF
    import qualified IO

    renderTextSolid :: TTF.Font -> String -> SDL.Color -> IO SDL.Surface
    renderTextSolid = TTF.renderTextSolid
        
    defaultFont :: Int -> IO TTF.Font
    defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
        
    withSDL :: IO () -> IO ()
    withSDL code = SDL.withInit [SDL.InitEverything] (IO.bracket TTF.init (const TTF.quit) (const code))

    type Color = (Word8, Word8, Word8)
    sdlColor :: Color -> SDL.Color
    sdlColor (r, g, b) = (SDL.Color r g b)
    sdlPixel :: SDL.Surface -> Color -> IO SDL.Pixel
    sdlPixel surface (r, g, b) = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

    getEvents :: IO [SDL.Event]
    getEvents = MyMonad.takeWhileM (return . (/=SDL.NoEvent)) SDL.pollEvent

    surfaceGetSize :: SDL.Surface -> (Int, Int)
    surfaceGetSize surface = (fromIntegral (SDL.surfaceGetWidth surface),
                              fromIntegral (SDL.surfaceGetHeight surface))

    data Vector2 a = Vector2 !a !a
      -- Note the Ord instance is obviously not a mathematical one
      -- (Vectors aren't ordinals!). Useful to have in a binary search
      -- tree though.
      deriving (Eq, Ord, Show, Read)

    makeRect :: Vector2 Int -> SDL.Rect
    makeRect (Vector2 x y) = SDL.Rect x y 0 0

    zipVectorWiths :: (a -> b -> c) -> (a -> b -> c) ->
                      Vector2 a -> Vector2 b -> Vector2 c
    zipVectorWiths f g (Vector2 ax ay) (Vector2 bx by) = Vector2 (f ax bx) (g ay by)

    zipVectorWith :: (a -> b -> c) -> Vector2 a -> Vector2 b -> Vector2 c
    zipVectorWith f = zipVectorWiths f f

    instance Functor Vector2 where
      fmap f (Vector2 x y) = Vector2 (f x) (f y)

    -- An improper Num instance, for convenience
    instance (Eq a, Show a, Num a) => Num (Vector2 a) where
      (+) = zipVectorWith (+)
      (-) = zipVectorWith (-)
      (*) = zipVectorWith (*)
      abs = fmap abs
      negate = fmap negate
      signum = fmap signum
      fromInteger x = let fi = fromInteger x in Vector2 fi fi
