{-# OPTIONS -Wall -O2 #-}

module MySDL where
    import Data.Word(Word8)
    import qualified MyMonad
    import qualified Graphics.UI.SDL as SDL
    import qualified Graphics.UI.SDL.TTF as TTF
    import qualified IO

    import qualified Graphics.UI.SDL.Utilities as Utils
    allValues :: (Bounded a, Utils.Enum a v) => [a]
    allValues = Utils.enumFromTo minBound maxBound

    renderText :: TTF.Font -> String -> SDL.Color -> IO SDL.Surface
    renderText font text color = if null text
                                 then
                                     SDL.createRGBSurface [] 0 0 0 0 0 0 0
                                 else
                                     TTF.renderTextBlended font text color

    textSize :: TTF.Font -> String -> IO (Vector2 Int)
    textSize font text = do
      (w, h) <- TTF.textSize font text
      return $ Vector2 w h

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

    makePosRect :: Vector2 Int -> SDL.Rect
    makePosRect (Vector2 x y) = SDL.Rect x y 0 0

    makeSizedRect :: Vector2 Int -> Vector2 Int -> SDL.Rect
    makeSizedRect (Vector2 x y) (Vector2 w h) = SDL.Rect x y w h

    rectX, rectY, rectW, rectH :: (Int -> Int) -> SDL.Rect -> SDL.Rect
    rectX modifyX (SDL.Rect x y w h) = SDL.Rect (modifyX x) y w h
    rectY modifyY (SDL.Rect x y w h) = SDL.Rect x (modifyY y) w h
    rectW modifyW (SDL.Rect x y w h) = SDL.Rect x y (modifyW w) h
    rectH modifyH (SDL.Rect x y w h) = SDL.Rect x y w (modifyH h)

    zipVectorWiths :: (a -> b -> c) -> (a -> b -> c) ->
                      Vector2 a -> Vector2 b -> Vector2 c
    zipVectorWiths f g (Vector2 ax ay) (Vector2 bx by) = Vector2 (f ax bx) (g ay by)

    zipVectorWith :: (a -> b -> c) -> Vector2 a -> Vector2 b -> Vector2 c
    zipVectorWith f = zipVectorWiths f f

    vectorX, vectorY :: (a -> a) -> Vector2 a -> Vector2 a
    vectorX modifyX (Vector2 x y) = Vector2 (modifyX x) y
    vectorY modifyY (Vector2 x y) = Vector2 x (modifyY y)

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
