{-# OPTIONS_GHC -Wall -O2 #-}

module MySDLKey(strOf, modsOf, noMods, Mods, shift, ctrl, alt,
                isShift, isCtrl, isAlt)
where

import qualified Data.Map as Map
import Graphics.UI.SDL.Keysym

data Mods = MkMods { isShift, isCtrl, isAlt :: Bool }
  deriving (Eq, Ord, Show, Read)

noMods, shift, ctrl, alt :: Mods
noMods = MkMods False False False
shift = noMods{isShift=True}
ctrl = noMods{isCtrl=True}
alt = noMods{isAlt=True}

modsOf :: [Modifier] -> Mods
modsOf mods =
    MkMods (any (`elem` mods)
            [KeyModLeftShift, KeyModRightShift, KeyModShift])
           (any (`elem` mods)
            [KeyModLeftCtrl, KeyModRightCtrl, KeyModCtrl])
           (any (`elem` mods)
            [KeyModLeftAlt, KeyModRightAlt, KeyModAlt])

strOf :: Mods -> SDLKey -> Maybe String
strOf mods key = (mods, key) `Map.lookup` keysMap

keysMap :: Map.Map (Mods, SDLKey) String
keysMap = Map.fromList [
  ((noMods, SDLK_SPACE), " "),
  ((noMods, SDLK_EXCLAIM), "!"),
  ((noMods, SDLK_QUOTEDBL), "\""),
  ((noMods, SDLK_HASH), "#"),
  ((noMods, SDLK_DOLLAR), "$"),
  ((noMods, SDLK_AMPERSAND), "&"),
  ((noMods, SDLK_QUOTE), "'"),
  ((noMods, SDLK_LEFTPAREN), "("),
  ((noMods, SDLK_RIGHTPAREN), ")"),
  ((noMods, SDLK_ASTERISK), "*"),
  ((noMods, SDLK_PLUS), "+"),
  ((noMods, SDLK_COMMA), ","),
  ((noMods, SDLK_MINUS), "-"),
  ((noMods, SDLK_PERIOD), "."),
  ((noMods, SDLK_SLASH), "/"),
  ((noMods, SDLK_0), "0"),
  ((noMods, SDLK_1), "1"),
  ((noMods, SDLK_2), "2"),
  ((noMods, SDLK_3), "3"),
  ((noMods, SDLK_4), "4"),
  ((noMods, SDLK_5), "5"),
  ((noMods, SDLK_6), "6"),
  ((noMods, SDLK_7), "7"),
  ((noMods, SDLK_8), "8"),
  ((noMods, SDLK_9), "9"),
  ((noMods, SDLK_COLON), ":"),
  ((noMods, SDLK_SEMICOLON), ";"),
  ((noMods, SDLK_LESS), "<"),
  ((noMods, SDLK_EQUALS), "="),
  ((noMods, SDLK_GREATER), ">"),
  ((noMods, SDLK_QUESTION), "?"),
  ((noMods, SDLK_AT), "@"),
  ((noMods, SDLK_LEFTBRACKET), "["),
  ((noMods, SDLK_BACKSLASH), "\\"),
  ((noMods, SDLK_RIGHTBRACKET), "]"),
  ((noMods, SDLK_UNDERSCORE), "_"),
  ((noMods, SDLK_BACKQUOTE), "`"),
  ((noMods, SDLK_a), "a"),
  ((noMods, SDLK_b), "b"),
  ((noMods, SDLK_c), "c"),
  ((noMods, SDLK_d), "d"),
  ((noMods, SDLK_e), "e"),
  ((noMods, SDLK_f), "f"),
  ((noMods, SDLK_g), "g"),
  ((noMods, SDLK_h), "h"),
  ((noMods, SDLK_i), "i"),
  ((noMods, SDLK_j), "j"),
  ((noMods, SDLK_k), "k"),
  ((noMods, SDLK_l), "l"),
  ((noMods, SDLK_m), "m"),
  ((noMods, SDLK_n), "n"),
  ((noMods, SDLK_o), "o"),
  ((noMods, SDLK_p), "p"),
  ((noMods, SDLK_q), "q"),
  ((noMods, SDLK_r), "r"),
  ((noMods, SDLK_s), "s"),
  ((noMods, SDLK_t), "t"),
  ((noMods, SDLK_u), "u"),
  ((noMods, SDLK_v), "v"),
  ((noMods, SDLK_w), "w"),
  ((noMods, SDLK_x), "x"),
  ((noMods, SDLK_y), "y"),
  ((noMods, SDLK_z), "z"),

  ((shift, SDLK_QUOTE), "\""),
  ((shift, SDLK_COMMA), "<"),
  ((shift, SDLK_MINUS), "_"),
  ((shift, SDLK_PERIOD), ">"),
  ((shift, SDLK_SLASH), "?"),
  ((shift, SDLK_0), ")"),
  ((shift, SDLK_1), "!"),
  ((shift, SDLK_2), "@"),
  ((shift, SDLK_3), "#"),
  ((shift, SDLK_4), "$"),
  ((shift, SDLK_5), "%"),
  ((shift, SDLK_6), "^"),
  ((shift, SDLK_7), "&"),
  ((shift, SDLK_8), "*"),
  ((shift, SDLK_9), "("),
  ((shift, SDLK_SEMICOLON), ":"),
  ((shift, SDLK_EQUALS), "+"),
  ((shift, SDLK_LEFTBRACKET), "{"),
  ((shift, SDLK_BACKSLASH), "|"),
  ((shift, SDLK_RIGHTBRACKET), "}"),
  ((shift, SDLK_BACKQUOTE), "~"),
  ((shift, SDLK_a), "A"),
  ((shift, SDLK_b), "B"),
  ((shift, SDLK_c), "C"),
  ((shift, SDLK_d), "D"),
  ((shift, SDLK_e), "E"),
  ((shift, SDLK_f), "F"),
  ((shift, SDLK_g), "G"),
  ((shift, SDLK_h), "H"),
  ((shift, SDLK_i), "I"),
  ((shift, SDLK_j), "J"),
  ((shift, SDLK_k), "K"),
  ((shift, SDLK_l), "L"),
  ((shift, SDLK_m), "M"),
  ((shift, SDLK_n), "N"),
  ((shift, SDLK_o), "O"),
  ((shift, SDLK_p), "P"),
  ((shift, SDLK_q), "Q"),
  ((shift, SDLK_r), "R"),
  ((shift, SDLK_s), "S"),
  ((shift, SDLK_t), "T"),
  ((shift, SDLK_u), "U"),
  ((shift, SDLK_v), "V"),
  ((shift, SDLK_w), "W"),
  ((shift, SDLK_x), "X"),
  ((shift, SDLK_y), "Y"),
  ((shift, SDLK_z), "Z")
  ]
