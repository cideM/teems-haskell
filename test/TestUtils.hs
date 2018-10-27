{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import           Text.Trifecta
import           Data.Map                      as DM
import           Lib

prs :: Parser a -> String -> Maybe a
prs p s =
  let r = parseString p mempty s
  in  case r of
        (Success a      ) -> Just a
        (Failure _) -> Nothing

testTheme :: Theme
testTheme = Theme "foo" $ DM.fromList
  [ ("foreground", "#ffffff")
  , ("background", "#bbbbbb")
  , ("color0"    , "#000000")
  , ("color1"    , "#111111")
  , ("color2"    , "#222222")
  , ("color3"    , "#333333")
  , ("color4"    , "#444444")
  , ("color5"    , "#555555")
  , ("color6"    , "#666666")
  , ("color7"    , "#777777")
  , ("color8"    , "#888888")
  , ("color9"    , "#999999")
  , ("color10"   , "#101010")
  , ("color11"   , "#111111")
  , ("color12"   , "#121212")
  , ("color13"   , "#131313")
  , ("color14"   , "#141414")
  , ("color15"   , "#151515")
  ]
