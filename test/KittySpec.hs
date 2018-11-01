{-# LANGUAGE OverloadedStrings #-}

module KittySpec where

import           TestUtils
import           Apps.Kitty
import           Lib
import           Test.Hspec

spec :: Spec
spec =
  describe "Kitty.configCreator"
    $ it "should replace colors with those from the theme"
    $ do
        let
          config
            = "foreground = #c5c8c6\n background = #1d1f21\n color0 = #1d1f21\n color8 = #969896\n color1 = #cc6666\n color9 = #cc6666\n color2 = #b5bd68\n color10 = #b5bd68\n color3 = #f0c674\n color11 = #f0c674\n color4 = #81a2be\n color12 = #81a2be\n color5 = #b294bb\n color13 = #b294bb\n color6 = #8abeb7\n color14 = #8abeb7\n color7 = #c5c8c6\n color15 = #ffffff\n"
        let
          expected
            = "foreground = #ffffff\n background = #323232\n color0 = #000000\n color8 = #080808\n color1 = #010101\n color9 = #090909\n color2 = #020202\n color10 = #0a0a0a\n color3 = #030303\n color11 = #0b0b0b\n color4 = #040404\n color12 = #0c0c0c\n color5 = #050505\n color13 = #0d0d0d\n color6 = #060606\n color14 = #0e0e0e\n color7 = #070707\n color15 = #0f0f0f\n"
        Lib.configCreator kitty testTheme config `shouldBe` Right expected
