{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import           Text.Trifecta
import           Data.Map                      as DM
import           Lib

prs :: Parser a -> String -> Maybe a
prs p s =
  let r = parseString p mempty s
  in  case r of
        (Success a) -> Just a
        (Failure _) -> Nothing

black :: ColorValue
black = ColorValue (HexColor "000000") (RGBAColor 0 0 0 0.0)

brightBlack :: ColorValue
brightBlack = ColorValue (HexColor "888888") (RGBAColor 88 88 88 0.8)

testTheme :: Theme
testTheme = Theme "foo" $ DM.fromList
  [ ( "foreground"
    , ColorValue (HexColor "ffffff") (RGBAColor 256 256 256 1.0)
    )
  , ( "background"
    , ColorValue (HexColor "bbbbbb") (RGBAColor 256 256 256 1.0)
    )
  , ( "color0"
    , black
    )
  , ( "color1"
    , ColorValue (HexColor "111111") (RGBAColor 256 256 256 1.0)
    )
  , ( "color2"
    , ColorValue (HexColor "222222") (RGBAColor 256 256 256 1.0)
    )
  , ( "color3"
    , ColorValue (HexColor "333333") (RGBAColor 256 256 256 1.0)
    )
  , ( "color4"
    , ColorValue (HexColor "444444") (RGBAColor 256 256 256 1.0)
    )
  , ( "color5"
    , ColorValue (HexColor "555555") (RGBAColor 256 256 256 1.0)
    )
  , ( "color6"
    , ColorValue (HexColor "666666") (RGBAColor 256 256 256 1.0)
    )
  , ( "color7"
    , ColorValue (HexColor "777777") (RGBAColor 256 256 256 1.0)
    )
  , ( "color8"
    , brightBlack
    )
  , ( "color9"
    , ColorValue (HexColor "999999") (RGBAColor 256 256 256 1.0)
    )
  , ( "color10"
    , ColorValue (HexColor "101010") (RGBAColor 256 256 256 1.0)
    )
  , ( "color11"
    , ColorValue (HexColor "111111") (RGBAColor 256 256 256 1.0)
    )
  , ( "color12"
    , ColorValue (HexColor "121212") (RGBAColor 256 256 256 1.0)
    )
  , ( "color13"
    , ColorValue (HexColor "131313") (RGBAColor 256 256 256 1.0)
    )
  , ( "color14"
    , ColorValue (HexColor "141414") (RGBAColor 256 256 256 1.0)
    )
  , ( "color15"
    , ColorValue (HexColor "151515") (RGBAColor 256 256 256 1.0)
    )
  ]
