{-# OPTIONS_GHC -Wall #-}

module MySDLKey(allValues, strOf, modsOf,
                Mods,
                isShift, isCtrl, isAlt)
where

import qualified Data.Map as Map
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Utilities as Utils
import Data.Char(toUpper)

allValues :: (Bounded a, Utils.Enum a v) => [a]
allValues = Utils.enumFromTo minBound maxBound

data Mods = MkMods { isShift, isCtrl, isAlt :: Bool }

modsOf :: [Modifier] -> Mods
modsOf mods =
    MkMods (any (`elem` mods)
            [KeyModLeftShift, KeyModRightShift, KeyModShift])
           (any (`elem` mods)
            [KeyModLeftCtrl, KeyModRightCtrl, KeyModCtrl])
           (any (`elem` mods)
            [KeyModLeftAlt, KeyModRightAlt, KeyModAlt])

strOf :: [Modifier] -> SDLKey -> Maybe String
strOf modifiers key =
    case modsOf modifiers of
      MkMods False False False -> id
      MkMods True False False -> (fmap . map) toUpper
      _ -> const Nothing
    $ key `Map.lookup` keysMap

keysMap :: Map.Map SDLKey String
keysMap = Map.fromList [
  (SDLK_SPACE, " "),
  (SDLK_EXCLAIM, "!"),
  (SDLK_QUOTEDBL, "\""),
  (SDLK_HASH, "#"),
  (SDLK_DOLLAR, "$"),
  (SDLK_AMPERSAND, "&"),
  (SDLK_QUOTE, "'"),
  (SDLK_LEFTPAREN, "("),
  (SDLK_RIGHTPAREN, ")"),
  (SDLK_ASTERISK, "*"),
  (SDLK_PLUS, "+"),
  (SDLK_COMMA, ","),
  (SDLK_MINUS, "-"),
  (SDLK_PERIOD, "."),
  (SDLK_SLASH, "/"),
  (SDLK_0, "0"),
  (SDLK_1, "1"),
  (SDLK_2, "2"),
  (SDLK_3, "3"),
  (SDLK_4, "4"),
  (SDLK_5, "5"),
  (SDLK_6, "6"),
  (SDLK_7, "7"),
  (SDLK_8, "8"),
  (SDLK_9, "9"),
  (SDLK_COLON, ":"),
  (SDLK_SEMICOLON, ";"),
  (SDLK_LESS, "<"),
  (SDLK_EQUALS, "="),
  (SDLK_GREATER, ">"),
  (SDLK_QUESTION, "?"),
  (SDLK_AT, "@"),
  (SDLK_LEFTBRACKET, "["),
  (SDLK_BACKSLASH, "\\"),
  (SDLK_RIGHTBRACKET, "]"),
  (SDLK_UNDERSCORE, "_"),
  (SDLK_BACKQUOTE, "`"),
  (SDLK_a, "a"),
  (SDLK_b, "b"),
  (SDLK_c, "c"),
  (SDLK_d, "d"),
  (SDLK_e, "e"),
  (SDLK_f, "f"),
  (SDLK_g, "g"),
  (SDLK_h, "h"),
  (SDLK_i, "i"),
  (SDLK_j, "j"),
  (SDLK_k, "k"),
  (SDLK_l, "l"),
  (SDLK_m, "m"),
  (SDLK_n, "n"),
  (SDLK_o, "o"),
  (SDLK_p, "p"),
  (SDLK_q, "q"),
  (SDLK_r, "r"),
  (SDLK_s, "s"),
  (SDLK_t, "t"),
  (SDLK_u, "u"),
  (SDLK_v, "v"),
  (SDLK_w, "w"),
  (SDLK_x, "x"),
  (SDLK_y, "y"),
  (SDLK_z, "z")
  ]
