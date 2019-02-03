{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module TermiteSpec where

import           Apps.Internal.Termite
import           Data.String.QQ
import           Test.Hspec
import           TestUtils
import           Types.Internal.Misc

spec :: Spec
spec =
  describe "configCreator" $ do
    it "should replace colors with those from the theme" $ do
      let config =
            [s|
      foreground = #c5c8c6
      background = #1d1f21
      color0 = #1d1f21
      color8 = #969896
      color1 = #cc6666
      color9 = #cc6666
      color2 = #b5bd68
      color10 = #b5bd68
      color3 = #f0c674
      color11 = #f0c674
      color4 = #81a2be
      color12 = #81a2be
      color5 = #b294bb
      color13 = #b294bb
      color6 = #8abeb7
      color14 = #8abeb7
      color7 = #c5c8c6
      color15 = #ffffff
|]
      let expected =
            [s|
      foreground = rgba(255, 255, 255, 1.0)
      background = rgba(50, 50, 50, 1.0)
      color0 = rgba(0, 0, 0, 1.0)
      color8 = rgba(8, 8, 8, 1.0)
      color1 = rgba(1, 1, 1, 1.0)
      color9 = rgba(9, 9, 9, 1.0)
      color2 = rgba(2, 2, 2, 1.0)
      color10 = rgba(10, 10, 10, 1.0)
      color3 = rgba(3, 3, 3, 1.0)
      color11 = rgba(11, 11, 11, 1.0)
      color4 = rgba(4, 4, 4, 1.0)
      color12 = rgba(12, 12, 12, 1.0)
      color5 = rgba(5, 5, 5, 1.0)
      color13 = rgba(13, 13, 13, 1.0)
      color6 = rgba(6, 6, 6, 1.0)
      color14 = rgba(14, 14, 14, 1.0)
      color7 = rgba(7, 7, 7, 1.0)
      color15 = rgba(15, 15, 15, 1.0)
|]
      _configCreator termite testTheme config `shouldBe` Right expected
    it "should handle rgba input" $ do
      let config =
            [s|
      foreground = rgba(1,1,1,1.0)
      background = rgba(1,1,1,1.0)
      color0 = rgba(1,1,1,1.0)
      color8 = rgba(1,1,1,1.0)
      color1 = rgba(1,1,1,1.0)
      color9 = rgba(1,1,1,1.0)
      color2 = rgba(1,1,1,1.0)
      color10 = rgba(1,1,1,1.0)
      color3 = rgba(1,1,1,1.0)
      color11 = rgba(1,1,1,1.0)
      color4 = rgba(1,1,1,1.0)
      color12 = rgba(1,1,1,1.0)
      color5 = rgba(1,1,1,1.0)
      color13 = rgba(1,1,1,1.0)
      color6 = rgba(1,1,1,1.0)
      color14 = rgba(1,1,1,1.0)
      color7 = rgba(1,1,1,1.0)
      color15 = rgba(1,1,1,1.0)
|]
      let expected =
            [s|
      foreground = rgba(255, 255, 255, 1.0)
      background = rgba(50, 50, 50, 1.0)
      color0 = rgba(0, 0, 0, 1.0)
      color8 = rgba(8, 8, 8, 1.0)
      color1 = rgba(1, 1, 1, 1.0)
      color9 = rgba(9, 9, 9, 1.0)
      color2 = rgba(2, 2, 2, 1.0)
      color10 = rgba(10, 10, 10, 1.0)
      color3 = rgba(3, 3, 3, 1.0)
      color11 = rgba(11, 11, 11, 1.0)
      color4 = rgba(4, 4, 4, 1.0)
      color12 = rgba(12, 12, 12, 1.0)
      color5 = rgba(5, 5, 5, 1.0)
      color13 = rgba(13, 13, 13, 1.0)
      color6 = rgba(6, 6, 6, 1.0)
      color14 = rgba(14, 14, 14, 1.0)
      color7 = rgba(7, 7, 7, 1.0)
      color15 = rgba(15, 15, 15, 1.0)
|]
      _configCreator termite testTheme config `shouldBe` Right expected
