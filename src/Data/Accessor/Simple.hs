{-# OPTIONS_GHC -Wall -O2 #-}

-- TODO: Put this in a more generic library

module Data.Accessor.Simple
     (reader) where

import Data.Accessor(Accessor, accessor)

reader :: r -> Accessor a r
reader x = accessor (const x) (const id)
