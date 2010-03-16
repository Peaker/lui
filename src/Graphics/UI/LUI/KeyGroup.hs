{-# OPTIONS -Wall -O2 -fno-warn-unused-imports #-}

module Graphics.UI.LUI.KeyGroup
    (KeyGroup, KeyGroupName, ModKey,
     -- The key groups
     noMods, shift, ctrl, alt,
     returnKey, escapeKey, backspaceKey, leftKey, rightKey,
     charKey, ctrlCharKey, homeKey, endKey, deleteKey,
     showMods, showModKey,
     unicodeOf, printables)
where

import Data.Char(toLower, toUpper, chr, ord)
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
import Control.Arrow(second)
import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT

type KeyGroupName = String
type ModKey = (GLUT.Modifiers, GLUT.Key)
type KeyGroup = (KeyGroupName, [ModKey])

unicodeOf :: ModKey -> Maybe Char
unicodeOf (mods, k) =
  case (c, a, k) of
    (GLUT.Up, GLUT.Up, GLUT.Char x) -> Just x
    _ -> Nothing
  where
    c = GLUT.ctrl mods
    a = GLUT.alt mods

printables :: [ModKey]
printables =
  [ (m, GLUT.Char l)
  | (m, ls) <- [(noMods, ['a'..'z'] ++ ['0'..'9'] ++ "`-=[]\\;',./+*")
               ,(shift, ['A'..'Z'] ++ "~!@#$%^&*()_+{}|<>?")]
  , l <- ls
  ]

noMods :: GLUT.Modifiers
noMods = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Up

shift :: GLUT.Modifiers
shift = noMods { GLUT.shift = GLUT.Down }

ctrl :: GLUT.Modifiers
ctrl = noMods { GLUT.ctrl = GLUT.Down }

alt :: GLUT.Modifiers
alt = noMods { GLUT.alt = GLUT.Down }

returnKey :: ModKey
returnKey = (noMods, GLUT.Char '\r')

escapeKey :: ModKey
escapeKey = (noMods, GLUT.Char '\x1B')

backspaceKey :: ModKey
backspaceKey = (noMods, GLUT.Char '\b')

leftKey :: ModKey
leftKey = (noMods, GLUT.SpecialKey GLUT.KeyLeft)

rightKey :: ModKey
rightKey = (noMods, GLUT.SpecialKey GLUT.KeyRight)

charKey :: ModKey
charKey = (noMods, GLUT.SpecialKey GLUT.KeyRight)

homeKey :: ModKey
homeKey = (noMods, GLUT.SpecialKey GLUT.KeyHome)

endKey :: ModKey
endKey = (noMods, GLUT.SpecialKey GLUT.KeyEnd)

deleteKey :: ModKey
deleteKey = (noMods, GLUT.Char '\DEL')

ctrlCharKey :: Char -> ModKey
ctrlCharKey c = (ctrl, GLUT.Char $ chr (ord (toLower c) + 1 - ord 'a'))

showMods :: GLUT.Modifiers -> String
showMods mods = intercalate "+" [ mName
                                | (ks, mName) <- modNames
                                , ks mods == GLUT.Down ]
  where
    modNames = [(GLUT.shift, "Shift"),
                (GLUT.ctrl, "Ctrl"),
                (GLUT.alt, "Alt")]

showModKey :: ModKey -> String
showModKey (mods, k) = showMods mods ++ " " ++ show k
