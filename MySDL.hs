{-# OPTIONS -Wall -O2 #-}

module MySDL where
    import Data.Word(Word8)
    import qualified MyMonad
    import qualified Graphics.UI.SDL as SDL
    import qualified Graphics.UI.SDL.TTF as TTF
    import qualified IO
    import Control.Exception(throwIO)
    import Control.Arrow(first, second)

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

    doNothing :: IO ()
    doNothing = return ()

    ioBoolToError :: String -> IO Bool -> IO ()
    ioBoolToError errStr act = do
      isSuccess <- act
      if isSuccess
        then return ()
        else throwIO . userError $ errStr

    initKeyRepeat :: IO ()
    initKeyRepeat = ioBoolToError "enableKeyRepeat failed" $ SDL.enableKeyRepeat 150 10

    bracket__ :: IO () -> IO () -> IO () -> IO ()
    bracket__ pre post code = IO.bracket_ pre (const post) code

    withSDL :: IO () -> IO ()
    withSDL = SDL.withInit [SDL.InitEverything] .
              bracket__ initKeyRepeat doNothing .
              bracket__ (ioBoolToError "TTF init failure" TTF.init) TTF.quit

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

    vector2first, vector2second :: Endo a -> Endo (Vector2 a)
    vector2first f (Vector2 x y) = Vector2 (f x) y
    vector2second f (Vector2 x y) = Vector2 x (f y)

    rectToVectors :: SDL.Rect -> (Vector2 Int, Vector2 Int)
    rectToVectors (SDL.Rect x y w h) = (Vector2 x y, Vector2 w h)
    makeRect :: Vector2 Int -> Vector2 Int -> SDL.Rect
    makeRect (Vector2 x y) (Vector2 w h) = SDL.Rect x y w h

    type Endo a = a -> a
    type Two a = (a, a)

    inRect :: Endo (Two (Vector2 Int)) -> Endo (SDL.Rect)
    inRect f = uncurry makeRect . f . rectToVectors

    rectPos, rectSize :: Endo (Vector2 Int) -> Endo (SDL.Rect)
    rectPos f = (inRect . first) f
    rectSize f = (inRect . second) f

    rectX, rectY, rectW, rectH :: Endo Int -> Endo (SDL.Rect)
    rectX = rectPos . vector2first
    rectY = rectPos . vector2second
    rectW = rectSize . vector2first
    rectH = rectSize . vector2second

    makePosRect :: Vector2 Int -> SDL.Rect
    makePosRect (Vector2 x y) = SDL.Rect x y 0 0

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
