{-# LANGUAGE OverloadedStrings #-}

module TermiteSpec where

import           TestUtils
import           Apps.Termite
import           Test.Hspec

spec :: Spec
spec = do
  describe "colorNameP" $ do
    it "should parse line with color"
      $          prs colorNameP "foreground = #ffffff"
      `shouldBe` Just "foreground"
    it "should not parse commented out line"
      $          prs colorNameP "# foreground = #ffffff"
      `shouldBe` Nothing
    it "should not parse line with wrong color name"
      $          prs colorNameP "# foo = #ffffff"
      `shouldBe` Nothing
  describe "configCreator'"
    $ it "should replace colors with those from the theme"
    $ do
        let
          config
            = "foreground = #c5c8c6\n background = #1d1f21\n color0 = #1d1f21\n color8 = #969896\n color1 = #cc6666\n color9 = #cc6666\n color2 = #b5bd68\n color10 = #b5bd68\n color3 = #f0c674\n color11 = #f0c674\n color4 = #81a2be\n color12 = #81a2be\n color5 = #b294bb\n color13 = #b294bb\n color6 = #8abeb7\n color14 = #8abeb7\n color7 = #c5c8c6\n color15 = #ffffff\n"
        let
          expected
            = "foreground = rgba(255, 255, 255, 1.0)\nbackground = rgba(50, 50, 50, 1.0)\ncolor0 = rgba(0, 0, 0, 1.0)\ncolor8 = rgba(8, 8, 8, 1.0)\ncolor1 = rgba(1, 1, 1, 1.0)\ncolor9 = rgba(9, 9, 9, 1.0)\ncolor2 = rgba(2, 2, 2, 1.0)\ncolor10 = rgba(10, 10, 10, 1.0)\ncolor3 = rgba(3, 3, 3, 1.0)\ncolor11 = rgba(11, 11, 11, 1.0)\ncolor4 = rgba(4, 4, 4, 1.0)\ncolor12 = rgba(12, 12, 12, 1.0)\ncolor5 = rgba(5, 5, 5, 1.0)\ncolor13 = rgba(13, 13, 13, 1.0)\ncolor6 = rgba(6, 6, 6, 1.0)\ncolor14 = rgba(14, 14, 14, 1.0)\ncolor7 = rgba(7, 7, 7, 1.0)\ncolor15 = rgba(15, 15, 15, 1.0)\n"
        configCreator' testTheme config `shouldBe` Right expected
