{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.LUI.Keymap
    (Keymap(keymapGroups),
     ModKey, -- re-export
     Doc,
     lookup, make, fromGroups, singleton, simpleton)
where

import Prelude hiding (lookup)
import Data.Monoid(Monoid(..))
import Graphics.UI.LUI.KeyGroup(KeyGroupName, ModKey, showModKey)

import qualified Data.Map as Map
import Data.Map(Map)

type Doc = String

data Keymap model = Keymap {
    keymapGroups :: Map KeyGroupName (Doc, Map ModKey model)
  , keymapCache :: Map ModKey (KeyGroupName, (Doc, model))
}

instance Functor Keymap where
  fmap f (Keymap groups cache) =
    Keymap ((fmap . fmap . fmap) f groups)
           ((fmap . fmap . fmap) f cache)

instance Monoid (Keymap model) where
  mempty = make Map.empty
  x `mappend` y = make $ keymapGroups x `mappend` keymapGroups y

lookup :: ModKey -> Keymap model -> Maybe (KeyGroupName, (Doc, model))
lookup modkey = Map.lookup modkey . keymapCache

make :: Map KeyGroupName (Doc, Map ModKey model) -> Keymap model
make handlers = Keymap handlers mkCache
  where
    mkCache = mconcat . Map.elems $ Map.mapWithKey putIntoMap handlers
    putIntoMap kgn (doc, modKeyToModel) =
      Map.map (\x -> (kgn, (doc, x))) modKeyToModel

fromGroups :: [(KeyGroupName, (Doc, Map ModKey model))] -> Keymap model
fromGroups = make . Map.fromList

singleton :: KeyGroupName -> Doc -> ModKey -> model -> Keymap model
singleton keyGroupName doc key model =
  make . Map.singleton keyGroupName $ (doc, Map.singleton key model)

simpleton :: ModKey -> Doc -> model -> Keymap model
simpleton key doc = singleton (showModKey key) doc key
