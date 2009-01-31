{-# OPTIONS -Wall -O2 #-}

module HaskGame.Color(Color(..))
where

import Data.Word(Word8)

data Color
  = Color {colorRed :: Word8,
           colorGreen :: Word8,
           colorBlue :: Word8}
