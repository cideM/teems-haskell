{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module XSpec where

import           Apps.Internal.X
import           Data.String.QQ
import           Test.Hspec
import           TestUtils
import           Types.Internal.Misc

spec :: Spec
spec =
  describe "configCreator" $
  it "should replace colors with those from the theme" $
  let config =
        [s|
      *.foreground: #c5c8c6
      *.background: #1d1f21
      *.color0: #1d1f21
      *.color8: #969896
      *.color1: #cc6666
      *.color9: #cc6666
      *.color2: #b5bd68
      *.color10: #b5bd68
      *.color3: #f0c674
      *.color11: #f0c674
      *.color4: #81a2be
      *.color12: #81a2be
      *.color5: #b294bb
      *.color13: #b294bb
      *.color6: #8abeb7
      *.color14: #8abeb7
      *.color7: #c5c8c6
      *.color15: #ffffff
|]
      expected =
        [s|
      *.foreground: #ffffff
      *.background: #323232
      *.color0: #000000
      *.color8: #080808
      *.color1: #010101
      *.color9: #090909
      *.color2: #020202
      *.color10: #0a0a0a
      *.color3: #030303
      *.color11: #0b0b0b
      *.color4: #040404
      *.color12: #0c0c0c
      *.color5: #050505
      *.color13: #0d0d0d
      *.color6: #060606
      *.color14: #0e0e0e
      *.color7: #070707
      *.color15: #0f0f0f
|]
   in _configCreator x testTheme config `shouldBe` Right expected
