{-# OPTIONS_GHC -Wall -O2 #-}

module MySDLKey(keyOfEvent
               , Key(..), KeyGroup(..)
               , singletonKeyGroup, inKeyGroup, keyName
               , Mods(..), noMods, shift, ctrl, alt)
where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Set as Set
import Func(result)

data Mods = MkMods { isShift, isCtrl, isAlt :: Bool }
  deriving (Eq, Ord, Show, Read)
data Key = Key Mods SDL.SDLKey
  deriving (Eq, Ord, Show)
data KeyGroup = KeyGroup {
      keyGroupName :: String,
      keyGroupKeys :: Set.Set Key
}
  deriving (Eq, Ord, Show)

singletonKeyGroup :: Key -> KeyGroup
singletonKeyGroup key = KeyGroup (keyName key) (Set.singleton key)

inKeyGroup :: Mods -> SDL.SDLKey -> KeyGroup
inKeyGroup = (result . result) singletonKeyGroup Key

modsName :: Mods -> String
modsName mods =
    let shiftStr = if isShift mods then "Shift+" else ""
        ctrlStr  = if isCtrl mods then "Ctrl+" else ""
        altStr   = if isAlt mods then "Alt+" else ""
    in concat [shiftStr, ctrlStr, altStr]

keyName :: Key -> String
keyName (Key mods sdlkey) = modsName mods ++ SDL.getKeyName sdlkey

noMods, shift, ctrl, alt :: Mods
noMods = MkMods False False False
shift = noMods{isShift=True}
ctrl = noMods{isCtrl=True}
alt = noMods{isAlt=True}

modsOf :: [SDL.Modifier] -> Mods
modsOf mods =
    MkMods (any (`elem` mods)
            [SDL.KeyModLeftShift,
             SDL.KeyModRightShift,
             SDL.KeyModShift])
           (any (`elem` mods)
            [SDL.KeyModLeftCtrl,
             SDL.KeyModRightCtrl,
             SDL.KeyModCtrl])
           (any (`elem` mods)
            [SDL.KeyModLeftAlt,
             SDL.KeyModRightAlt,
             SDL.KeyModAlt])

keyOfEvent :: SDL.Keysym -> Key
keyOfEvent keySym = Key (modsOf $ SDL.symModifiers keySym)
                        (SDL.symKey keySym)
