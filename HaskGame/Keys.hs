{-# OPTIONS_GHC -Wall -O2 #-}

module HaskGame.Keys
    (KeyGroup(..),allGroups,groupsOfKey,keysUnicode
    ,printableGroup,digitsGroup,lettersGroup
    ,upperCaseGroup,lowerCaseGroup,arrowsGroup
    )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import HaskGame.Key(noMods, shift, ModKey(..), KeyGroup(..)
                   ,singletonKeyGroup)

inGroup :: ModKey -> KeyGroup -> Bool
key `inGroup` group = key `Set.member` (keyGroupKeys group)

groupsOfKey :: ModKey -> [KeyGroup]
groupsOfKey key = singletonKeyGroup key :
                  filter (key `inGroup`) allGroups

allGroups :: [KeyGroup]
allGroups =
    [printableGroup
    ,digitsGroup
    ,lettersGroup
    ,upperCaseGroup
    ,lowerCaseGroup
    ,arrowsGroup
    ]

combineGroups :: String -> [KeyGroup] -> KeyGroup
combineGroups name groups = KeyGroup name (Set.unions . map keyGroupKeys $ groups)

keysSetOfUnicode :: Map.Map ModKey String -> Set.Set ModKey
keysSetOfUnicode = Set.fromList . Map.keys

printableGroup, digitsGroup, lettersGroup,
  upperCaseGroup, lowerCaseGroup, arrowsGroup :: KeyGroup
lettersGroup = combineGroups "Letters" [upperCaseGroup, lowerCaseGroup]
printableGroup = KeyGroup "Printable" . keysSetOfUnicode $ keysUnicode
digitsGroup = KeyGroup "Digits" . keysSetOfUnicode $ digitsUnicode
upperCaseGroup = KeyGroup "Upper-case letters" . keysSetOfUnicode $ upperCaseUnicode
lowerCaseGroup = KeyGroup "Lower-case letters" . keysSetOfUnicode $ lowerCaseUnicode
arrowsGroup = KeyGroup "Arrows" $
              Set.fromList [ModKey noMods SDL.SDLK_LEFT
                           ,ModKey noMods SDL.SDLK_RIGHT
                           ,ModKey noMods SDL.SDLK_UP
                           ,ModKey noMods SDL.SDLK_DOWN]

lowerCaseUnicode, upperCaseUnicode, digitsUnicode :: Map.Map ModKey String
lowerCaseUnicode = Map.fromList $
    [(ModKey noMods SDL.SDLK_a, "a")
    ,(ModKey noMods SDL.SDLK_b, "b")
    ,(ModKey noMods SDL.SDLK_c, "c")
    ,(ModKey noMods SDL.SDLK_d, "d")
    ,(ModKey noMods SDL.SDLK_e, "e")
    ,(ModKey noMods SDL.SDLK_f, "f")
    ,(ModKey noMods SDL.SDLK_g, "g")
    ,(ModKey noMods SDL.SDLK_h, "h")
    ,(ModKey noMods SDL.SDLK_i, "i")
    ,(ModKey noMods SDL.SDLK_j, "j")
    ,(ModKey noMods SDL.SDLK_k, "k")
    ,(ModKey noMods SDL.SDLK_l, "l")
    ,(ModKey noMods SDL.SDLK_m, "m")
    ,(ModKey noMods SDL.SDLK_n, "n")
    ,(ModKey noMods SDL.SDLK_o, "o")
    ,(ModKey noMods SDL.SDLK_p, "p")
    ,(ModKey noMods SDL.SDLK_q, "q")
    ,(ModKey noMods SDL.SDLK_r, "r")
    ,(ModKey noMods SDL.SDLK_s, "s")
    ,(ModKey noMods SDL.SDLK_t, "t")
    ,(ModKey noMods SDL.SDLK_u, "u")
    ,(ModKey noMods SDL.SDLK_v, "v")
    ,(ModKey noMods SDL.SDLK_w, "w")
    ,(ModKey noMods SDL.SDLK_x, "x")
    ,(ModKey noMods SDL.SDLK_y, "y")
    ,(ModKey noMods SDL.SDLK_z, "z")
    ]
upperCaseUnicode = Map.fromList $
    [(ModKey shift SDL.SDLK_a, "A")
    ,(ModKey shift SDL.SDLK_b, "B")
    ,(ModKey shift SDL.SDLK_c, "C")
    ,(ModKey shift SDL.SDLK_d, "D")
    ,(ModKey shift SDL.SDLK_e, "E")
    ,(ModKey shift SDL.SDLK_f, "F")
    ,(ModKey shift SDL.SDLK_g, "G")
    ,(ModKey shift SDL.SDLK_h, "H")
    ,(ModKey shift SDL.SDLK_i, "I")
    ,(ModKey shift SDL.SDLK_j, "J")
    ,(ModKey shift SDL.SDLK_k, "K")
    ,(ModKey shift SDL.SDLK_l, "L")
    ,(ModKey shift SDL.SDLK_m, "M")
    ,(ModKey shift SDL.SDLK_n, "N")
    ,(ModKey shift SDL.SDLK_o, "O")
    ,(ModKey shift SDL.SDLK_p, "P")
    ,(ModKey shift SDL.SDLK_q, "Q")
    ,(ModKey shift SDL.SDLK_r, "R")
    ,(ModKey shift SDL.SDLK_s, "S")
    ,(ModKey shift SDL.SDLK_t, "T")
    ,(ModKey shift SDL.SDLK_u, "U")
    ,(ModKey shift SDL.SDLK_v, "V")
    ,(ModKey shift SDL.SDLK_w, "W")
    ,(ModKey shift SDL.SDLK_x, "X")
    ,(ModKey shift SDL.SDLK_y, "Y")
    ,(ModKey shift SDL.SDLK_z, "Z")
    ]
digitsUnicode = Map.fromList $
    [(ModKey noMods SDL.SDLK_0, "0")
    ,(ModKey noMods SDL.SDLK_1, "1")
    ,(ModKey noMods SDL.SDLK_2, "2")
    ,(ModKey noMods SDL.SDLK_3, "3")
    ,(ModKey noMods SDL.SDLK_4, "4")
    ,(ModKey noMods SDL.SDLK_5, "5")
    ,(ModKey noMods SDL.SDLK_6, "6")
    ,(ModKey noMods SDL.SDLK_7, "7")
    ,(ModKey noMods SDL.SDLK_8, "8")
    ,(ModKey noMods SDL.SDLK_9, "9")
    ,(ModKey shift SDL.SDLK_KP0, "0")
    ,(ModKey shift SDL.SDLK_KP1, "1")
    ,(ModKey shift SDL.SDLK_KP2, "2")
    ,(ModKey shift SDL.SDLK_KP3, "3")
    ,(ModKey shift SDL.SDLK_KP4, "4")
    ,(ModKey shift SDL.SDLK_KP5, "5")
    ,(ModKey shift SDL.SDLK_KP6, "6")
    ,(ModKey shift SDL.SDLK_KP7, "7")
    ,(ModKey shift SDL.SDLK_KP8, "8")
    ,(ModKey shift SDL.SDLK_KP9, "9")
    ]

keysUnicode :: Map.Map ModKey String
keysUnicode = Map.unions
              [lowerCaseUnicode
              ,upperCaseUnicode
              ,digitsUnicode,
               Map.fromList
               [(ModKey noMods SDL.SDLK_SPACE, " ")
               ,(ModKey noMods SDL.SDLK_EXCLAIM, "!")
               ,(ModKey noMods SDL.SDLK_QUOTEDBL, "\"")
               ,(ModKey noMods SDL.SDLK_HASH, "#")
               ,(ModKey noMods SDL.SDLK_DOLLAR, "$")
               ,(ModKey noMods SDL.SDLK_AMPERSAND, "&")
               ,(ModKey noMods SDL.SDLK_QUOTE, "'")
               ,(ModKey noMods SDL.SDLK_LEFTPAREN, "(")
               ,(ModKey noMods SDL.SDLK_RIGHTPAREN, ")")
               ,(ModKey noMods SDL.SDLK_ASTERISK, "*")
               ,(ModKey noMods SDL.SDLK_PLUS, "+")
               ,(ModKey noMods SDL.SDLK_COMMA, ",")
               ,(ModKey noMods SDL.SDLK_MINUS, "-")
               ,(ModKey noMods SDL.SDLK_PERIOD, ".")
               ,(ModKey noMods SDL.SDLK_SLASH, "/")
               ,(ModKey noMods SDL.SDLK_COLON, ":")
               ,(ModKey noMods SDL.SDLK_SEMICOLON, ";")
               ,(ModKey noMods SDL.SDLK_LESS, "<")
               ,(ModKey noMods SDL.SDLK_EQUALS, "=")
               ,(ModKey noMods SDL.SDLK_GREATER, ">")
               ,(ModKey noMods SDL.SDLK_QUESTION, "?")
               ,(ModKey noMods SDL.SDLK_AT, "@")
               ,(ModKey noMods SDL.SDLK_LEFTBRACKET, "[")
               ,(ModKey noMods SDL.SDLK_BACKSLASH, "\\")
               ,(ModKey noMods SDL.SDLK_RIGHTBRACKET, "]")
               ,(ModKey noMods SDL.SDLK_UNDERSCORE, "_")
               ,(ModKey noMods SDL.SDLK_BACKQUOTE, "`")

               ,(ModKey shift SDL.SDLK_QUOTE, "\"")
               ,(ModKey shift SDL.SDLK_COMMA, "<")
               ,(ModKey shift SDL.SDLK_MINUS, "_")
               ,(ModKey shift SDL.SDLK_PERIOD, ">")
               ,(ModKey shift SDL.SDLK_SLASH, "?")
               ,(ModKey shift SDL.SDLK_0, ")")
               ,(ModKey shift SDL.SDLK_1, "!")
               ,(ModKey shift SDL.SDLK_2, "@")
               ,(ModKey shift SDL.SDLK_3, "#")
               ,(ModKey shift SDL.SDLK_4, "$")
               ,(ModKey shift SDL.SDLK_5, "%")
               ,(ModKey shift SDL.SDLK_6, "^")
               ,(ModKey shift SDL.SDLK_7, "&")
               ,(ModKey shift SDL.SDLK_8, "*")
               ,(ModKey shift SDL.SDLK_9, "(")
               ,(ModKey shift SDL.SDLK_SEMICOLON, ":")
               ,(ModKey shift SDL.SDLK_EQUALS, "+")
               ,(ModKey shift SDL.SDLK_LEFTBRACKET, "{")
               ,(ModKey shift SDL.SDLK_BACKSLASH, "|")
               ,(ModKey shift SDL.SDLK_RIGHTBRACKET, "}")
               ,(ModKey shift SDL.SDLK_BACKQUOTE, "~")
               ,(ModKey shift SDL.SDLK_KP_PERIOD, ".")
               ,(ModKey shift SDL.SDLK_KP_DIVIDE, "/")
               ,(ModKey shift SDL.SDLK_KP_MULTIPLY, "*")
               ,(ModKey shift SDL.SDLK_KP_MINUS, "-")
               ,(ModKey shift SDL.SDLK_KP_PLUS, "+")
               ,(ModKey shift SDL.SDLK_KP_EQUALS, "=")
               ]]
