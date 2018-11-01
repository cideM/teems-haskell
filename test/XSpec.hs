{-# LANGUAGE OverloadedStrings #-}

module XSpec where

import           TestUtils
import           Apps.X
import           Lib
import           Test.Hspec

spec :: Spec
spec = do
  describe "resourceP"
    $          it "should parse a color name"
    $          prs resourceP "color1"
    `shouldBe` Just "color1"
  describe "nameClassP" $ do
    it "should parse *" $ prs (nameClassP ["*"]) "*color4" `shouldBe` Just "*"
    it "should parse *."
      $          prs (nameClassP ["*", "*."]) "*.color4"
      `shouldBe` Just "*."
    it "should parse xterm."
      $          prs (nameClassP ["*", "*.", "xterm."]) "xterm.color4"
      `shouldBe` Just "xterm."
    it "should parse xterm.VTE100."
      $ prs (nameClassP ["*", "*.", "xterm.VTE100."]) "xterm.VTE100.color4"
      `shouldBe` Just "xterm.VTE100."
  describe "xLineP" $ do
    it "should parse line with wildcard"
      $          prs (xLineP ["*"]) "*color1: #000000"
      `shouldBe` Just "color1"
    it "should not parse line that does not match exactly"
      $          prs (xLineP ["*"]) "XTerm*color1: #000000"
      `shouldBe` Nothing
    it "should parse line with app name"
      $          prs (xLineP ["xterm."]) "xterm.color1: #000000"
      `shouldBe` Just "color1"
    it "should not parse line with wrong app name"
      $          prs (xLineP ["foo"]) "xterm*color1: #000000"
      `shouldBe` Nothing
    it "should maintain whitespace"
      $          prs (xLineP ["*"]) "*color1   :   #000000"
      `shouldBe` Just "color1"
  describe "X.configCreator"
    $ it "should replace colors with those from the theme"
    $ do
        let
          config
            = "*.foreground: #c5c8c6\n *.background: #1d1f21\n *.color0: #1d1f21\n *.color8: #969896\n *.color1: #cc6666\n *.color9: #cc6666\n *.color2: #b5bd68\n *.color10: #b5bd68\n *.color3: #f0c674\n *.color11: #f0c674\n *.color4: #81a2be\n *.color12: #81a2be\n *.color5: #b294bb\n *.color13: #b294bb\n *.color6: #8abeb7\n *.color14: #8abeb7\n *.color7: #c5c8c6\n *.color15: #ffffff\n"
        let
          expected
            = "*.foreground: #ffffff\n *.background: #323232\n *.color0: #000000\n *.color8: #080808\n *.color1: #010101\n *.color9: #090909\n *.color2: #020202\n *.color10: #0a0a0a\n *.color3: #030303\n *.color11: #0b0b0b\n *.color4: #040404\n *.color12: #0c0c0c\n *.color5: #050505\n *.color13: #0d0d0d\n *.color6: #060606\n *.color14: #0e0e0e\n *.color7: #070707\n *.color15: #0f0f0f\n"
        Lib.configCreator x testTheme config `shouldBe` Right expected
