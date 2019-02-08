{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AlacrittySpec where

import           Apps.Internal.Alacritty (alacritty)
import           Data.String.QQ
import           Test.Hspec
import           TestUtils
import           Types.Internal.Misc

spec :: Spec
spec =
  describe "alacritty" $
  it "should return a new config based on the theme" $ do
    let config =
          [s|
cursor:
  text:         '0x000000'
  cursor:       '0x000000'

primary:
  background:     '0x000000'
  foreground:     '0x000000'

# Normal
normal:
  black:       '0x000000'
  red:         '0x000000'
  green:       '0x000000'
  yellow:      '0x000000'
  blue:        '0x000000'
  magenta:     '0x000000'
  cyan:        '0x000000'
  white:       '0x000000'

# Bright colors
bright:
  black:       '0x000000'
  red:         '0x000000'
  green:       '0x000000'
  yellow:      '0x000000'
  blue:        '0x000000'
  magenta:     '0x000000'
  cyan:        '0x000000'
  white:       '0x000000'
|]
    let expected =
          [s|
cursor:
  text:         '0x0c0c0c'
  cursor:       '0x171717'

primary:
  background:     '0x323232'
  foreground:     '0xffffff'

# Normal
normal:
  black:       '0x000000'
  red:         '0x010101'
  green:       '0x020202'
  yellow:      '0x030303'
  blue:        '0x040404'
  magenta:     '0x050505'
  cyan:        '0x060606'
  white:       '0x070707'

# Bright colors
bright:
  black:       '0x080808'
  red:         '0x090909'
  green:       '0x0a0a0a'
  yellow:      '0x0b0b0b'
  blue:        '0x0c0c0c'
  magenta:     '0x0d0d0d'
  cyan:        '0x0e0e0e'
  white:       '0x0f0f0f'
|]
    _configCreator alacritty testTheme config `shouldBe` Right expected
