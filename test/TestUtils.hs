{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import           Text.Trifecta
import           Data.Map                      as Map
import           Types

prs :: Parser a -> String -> Maybe a
prs p s =
  let r = parseString p mempty s
  in  case r of
        (Success a) -> Just a
        (Failure _) -> Nothing

black :: RGBA
black = RGBA 0 0 0 1

brightBlack :: RGBA
brightBlack = RGBA 8 8 8 1

testTheme :: Theme
testTheme = Theme "foo" $ Map.fromList
  [ ("foreground", RGBA 255 255 255 1)
  , ("background", RGBA 50 50 50 1)
  , ("color0"    , black)
  , ("color1"    , RGBA 1 1 1 1)
  , ("color2"    , RGBA 2 2 2 1)
  , ("color3"    , RGBA 3 3 3 1)
  , ("color4"    , RGBA 4 4 4 1)
  , ("color5"    , RGBA 5 5 5 1)
  , ("color6"    , RGBA 6 6 6 1)
  , ("color7"    , RGBA 7 7 7 1)
  , ("color8"    , brightBlack)
  , ("color9"    , RGBA 9 9 9 1)
  , ("color10"   , RGBA 10 10 10 1)
  , ("color11"   , RGBA 11 11 11 1)
  , ("color12"   , RGBA 12 12 12 1)
  , ("color13"   , RGBA 13 13 13 1)
  , ("color14"   , RGBA 14 14 14 1)
  , ("color15"   , RGBA 15 15 15 1)
  ]
