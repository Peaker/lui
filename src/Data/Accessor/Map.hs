{-# OPTIONS_GHC -Wall -O2 #-}

-- TODO: Put this in a more generic library

module Data.Accessor.Map
     (aMapValue
     ,aMapValueDefault) where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Accessor(Accessor, accessor)

aMapValue :: Ord k => k -> Accessor (Map k a) a
aMapValue key = accessor (Map.! key) setValue
    where
      setValue value = Map.adjust (const value) key

aMapValueDefault :: Ord k => a -> k -> Accessor (Map k a) a
aMapValueDefault def key = accessor (Map.findWithDefault def key) (Map.insert key)
