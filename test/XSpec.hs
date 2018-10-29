{-# LANGUAGE OverloadedStrings #-}

module XSpec where

import           TestUtils
import           X
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
      `shouldBe` Just XLine {_leading = "*color1: ", _color = "color1"}
    it "should not parse line that does not match exactly"
      $          prs (xLineP ["*"]) "XTerm*color1: #000000"
      `shouldBe` Nothing
    it "should parse line with app name"
      $          prs (xLineP ["xterm."]) "xterm.color1: #000000"
      `shouldBe` Just XLine {_leading = "xterm.color1: ", _color = "color1"}
    it "should not parse line with wrong app name"
      $          prs (xLineP ["foo"]) "xterm*color1: #000000"
      `shouldBe` Nothing
    it "should maintain whitespace"
      $          prs (xLineP ["*"]) "*color1   :   #000000"
      `shouldBe` Just XLine {_leading = "*color1   :   ", _color = "color1"}
  describe "makeNewLine"
    $          it "should construct line"
    $          makeNewLine black (XLine "xterm*color1: " "color1")
    `shouldBe` "xterm*color1: #000000"
  describe "configCreator'"
    $ it "should replace colors with those from the theme"
    $ do
        let
          config
            = "*.foreground: #c5c8c6\n *.background: #1d1f21\n *.color0: #1d1f21\n *.color8: #969896\n *.color1: #cc6666\n *.color9: #cc6666\n *.color2: #b5bd68\n *.color10: #b5bd68\n *.color3: #f0c674\n *.color11: #f0c674\n *.color4: #81a2be\n *.color12: #81a2be\n *.color5: #b294bb\n *.color13: #b294bb\n *.color6: #8abeb7\n *.color14: #8abeb7\n *.color7: #c5c8c6\n *.color15: #ffffff\n"
        let
          expected
            = "*.foreground: #ffffff\n *.background: #bbbbbb\n *.color0: #000000\n *.color8: #888888\n *.color1: #111111\n *.color9: #999999\n *.color2: #222222\n *.color10: #101010\n *.color3: #333333\n *.color11: #111111\n *.color4: #444444\n *.color12: #121212\n *.color5: #555555\n *.color13: #131313\n *.color6: #666666\n *.color14: #141414\n *.color7: #777777\n *.color15: #151515\n"
        configCreator' ["*."] testTheme config `shouldBe` expected
