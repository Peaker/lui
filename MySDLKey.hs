{-# OPTIONS_GHC -Wall -O2 #-}

module MySDLKey(strOf, keyOfEvent, noMods, Key(..), Mods, shift, ctrl, alt,
                isShift, isCtrl, isAlt)
where

import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL

data Mods = MkMods { isShift, isCtrl, isAlt :: Bool }
  deriving (Eq, Ord, Show, Read)
data Key = Key Mods SDL.SDLKey
  deriving (Eq, Ord, Show)

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

strOf :: Key -> Maybe String
strOf key = key `Map.lookup` keysMap

keysMap :: Map.Map Key String
keysMap = Map.fromList [
  (Key noMods SDL.SDLK_SPACE, " "),
  (Key noMods SDL.SDLK_EXCLAIM, "!"),
  (Key noMods SDL.SDLK_QUOTEDBL, "\""),
  (Key noMods SDL.SDLK_HASH, "#"),
  (Key noMods SDL.SDLK_DOLLAR, "$"),
  (Key noMods SDL.SDLK_AMPERSAND, "&"),
  (Key noMods SDL.SDLK_QUOTE, "'"),
  (Key noMods SDL.SDLK_LEFTPAREN, "("),
  (Key noMods SDL.SDLK_RIGHTPAREN, ")"),
  (Key noMods SDL.SDLK_ASTERISK, "*"),
  (Key noMods SDL.SDLK_PLUS, "+"),
  (Key noMods SDL.SDLK_COMMA, ","),
  (Key noMods SDL.SDLK_MINUS, "-"),
  (Key noMods SDL.SDLK_PERIOD, "."),
  (Key noMods SDL.SDLK_SLASH, "/"),
  (Key noMods SDL.SDLK_0, "0"),
  (Key noMods SDL.SDLK_1, "1"),
  (Key noMods SDL.SDLK_2, "2"),
  (Key noMods SDL.SDLK_3, "3"),
  (Key noMods SDL.SDLK_4, "4"),
  (Key noMods SDL.SDLK_5, "5"),
  (Key noMods SDL.SDLK_6, "6"),
  (Key noMods SDL.SDLK_7, "7"),
  (Key noMods SDL.SDLK_8, "8"),
  (Key noMods SDL.SDLK_9, "9"),
  (Key noMods SDL.SDLK_COLON, ":"),
  (Key noMods SDL.SDLK_SEMICOLON, ";"),
  (Key noMods SDL.SDLK_LESS, "<"),
  (Key noMods SDL.SDLK_EQUALS, "="),
  (Key noMods SDL.SDLK_GREATER, ">"),
  (Key noMods SDL.SDLK_QUESTION, "?"),
  (Key noMods SDL.SDLK_AT, "@"),
  (Key noMods SDL.SDLK_LEFTBRACKET, "["),
  (Key noMods SDL.SDLK_BACKSLASH, "\\"),
  (Key noMods SDL.SDLK_RIGHTBRACKET, "]"),
  (Key noMods SDL.SDLK_UNDERSCORE, "_"),
  (Key noMods SDL.SDLK_BACKQUOTE, "`"),
  (Key noMods SDL.SDLK_a, "a"),
  (Key noMods SDL.SDLK_b, "b"),
  (Key noMods SDL.SDLK_c, "c"),
  (Key noMods SDL.SDLK_d, "d"),
  (Key noMods SDL.SDLK_e, "e"),
  (Key noMods SDL.SDLK_f, "f"),
  (Key noMods SDL.SDLK_g, "g"),
  (Key noMods SDL.SDLK_h, "h"),
  (Key noMods SDL.SDLK_i, "i"),
  (Key noMods SDL.SDLK_j, "j"),
  (Key noMods SDL.SDLK_k, "k"),
  (Key noMods SDL.SDLK_l, "l"),
  (Key noMods SDL.SDLK_m, "m"),
  (Key noMods SDL.SDLK_n, "n"),
  (Key noMods SDL.SDLK_o, "o"),
  (Key noMods SDL.SDLK_p, "p"),
  (Key noMods SDL.SDLK_q, "q"),
  (Key noMods SDL.SDLK_r, "r"),
  (Key noMods SDL.SDLK_s, "s"),
  (Key noMods SDL.SDLK_t, "t"),
  (Key noMods SDL.SDLK_u, "u"),
  (Key noMods SDL.SDLK_v, "v"),
  (Key noMods SDL.SDLK_w, "w"),
  (Key noMods SDL.SDLK_x, "x"),
  (Key noMods SDL.SDLK_y, "y"),
  (Key noMods SDL.SDLK_z, "z"),

  (Key shift SDL.SDLK_QUOTE, "\""),
  (Key shift SDL.SDLK_COMMA, "<"),
  (Key shift SDL.SDLK_MINUS, "_"),
  (Key shift SDL.SDLK_PERIOD, ">"),
  (Key shift SDL.SDLK_SLASH, "?"),
  (Key shift SDL.SDLK_0, ")"),
  (Key shift SDL.SDLK_1, "!"),
  (Key shift SDL.SDLK_2, "@"),
  (Key shift SDL.SDLK_3, "#"),
  (Key shift SDL.SDLK_4, "$"),
  (Key shift SDL.SDLK_5, "%"),
  (Key shift SDL.SDLK_6, "^"),
  (Key shift SDL.SDLK_7, "&"),
  (Key shift SDL.SDLK_8, "*"),
  (Key shift SDL.SDLK_9, "("),
  (Key shift SDL.SDLK_SEMICOLON, ":"),
  (Key shift SDL.SDLK_EQUALS, "+"),
  (Key shift SDL.SDLK_LEFTBRACKET, "{"),
  (Key shift SDL.SDLK_BACKSLASH, "|"),
  (Key shift SDL.SDLK_RIGHTBRACKET, "}"),
  (Key shift SDL.SDLK_BACKQUOTE, "~"),
  (Key shift SDL.SDLK_a, "A"),
  (Key shift SDL.SDLK_b, "B"),
  (Key shift SDL.SDLK_c, "C"),
  (Key shift SDL.SDLK_d, "D"),
  (Key shift SDL.SDLK_e, "E"),
  (Key shift SDL.SDLK_f, "F"),
  (Key shift SDL.SDLK_g, "G"),
  (Key shift SDL.SDLK_h, "H"),
  (Key shift SDL.SDLK_i, "I"),
  (Key shift SDL.SDLK_j, "J"),
  (Key shift SDL.SDLK_k, "K"),
  (Key shift SDL.SDLK_l, "L"),
  (Key shift SDL.SDLK_m, "M"),
  (Key shift SDL.SDLK_n, "N"),
  (Key shift SDL.SDLK_o, "O"),
  (Key shift SDL.SDLK_p, "P"),
  (Key shift SDL.SDLK_q, "Q"),
  (Key shift SDL.SDLK_r, "R"),
  (Key shift SDL.SDLK_s, "S"),
  (Key shift SDL.SDLK_t, "T"),
  (Key shift SDL.SDLK_u, "U"),
  (Key shift SDL.SDLK_v, "V"),
  (Key shift SDL.SDLK_w, "W"),
  (Key shift SDL.SDLK_x, "X"),
  (Key shift SDL.SDLK_y, "Y"),
  (Key shift SDL.SDLK_z, "Z")
  ]
