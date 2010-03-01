{-# OPTIONS_GHC -Wall -O2 #-}

-- TODO: Put this in a more generic library

module Data.Accessor.List
     (anth) where

import Data.Editor.List(nth)
import Data.Accessor(Accessor, accessor)

anth :: Int -> Accessor [a] a
anth n = accessor (!!n) (nth n . const)
