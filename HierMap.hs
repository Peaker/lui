{-# OPTIONS_GHC -Wall #-}

module HierMap(HierMap, simpleHierMap, lookup) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Monad(msum)

data HierMap k a = MkHierMap [HierMap k a] (Map.Map k a) [HierMap k a]
simpleHierMap :: Map.Map k a -> HierMap k a
simpleHierMap m = MkHierMap [] m []

maps :: HierMap k a -> [Map.Map k a]
maps (MkHierMap preMaps hmap postMaps) = concat [ concatMap maps preMaps,
                                                  [hmap],
                                                  concatMap maps postMaps ]

lookup :: Ord k => k -> HierMap k a -> Maybe a
lookup k m = msum . map (Map.lookup k) $ maps m
