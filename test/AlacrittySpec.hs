{-# LANGUAGE OverloadedStrings #-}

module AlacrittySpec where

import           Apps.Internal.Alacritty
import           Test.Hspec
import           Types
import           TestUtils

spec :: Spec
spec = do
  describe "modeP" $ do
    it "should parse bright"
      $          prs modeP "bright: # this is just foo"
      `shouldBe` Just (Mode Bright)
    it "should parse normal"
      $          prs modeP "normal: # this is just foo"
      `shouldBe` Just (Mode Normal)
  describe "colorP" $ do
    it "should parse lines with leading spaces"
      $          prs colorP "black: '0x000000'"
      `shouldBe` Just (ColorName "black")
    it "should parse lines without leading spaces"
      $          prs colorP "black: '0x000000'"
      `shouldBe` Just (ColorName "black")
    it "should skip commented lines"
      $          prs colorP "#   black: '0x000000'"
      `shouldBe` Nothing
  describe "getVal" $ do
    it "should return normal colors"
      $          getVal testTheme Normal "black"
      `shouldBe` Just black
    it "should return bright colors"
      $          getVal testTheme Bright "black"
      `shouldBe` Just brightBlack
  describe "configCreator'"
    $ it "should return a new config based on the theme"
    $ do
        let
          config
            = "background:     '0x000000'\n foreground:     '0x000000'\n \n normal:\n black:       '0x000000'\n red:         '0x000000'\n green:       '0x000000'\n yellow:      '0x000000'\n blue:        '0x000000'\n magenta:     '0x000000'\n cyan:        '0x000000'\n white:       '0x000000'\n \n # Bright colors\n bright:\n black:       '0x000000'\n red:         '0x000000'\n green:       '0x000000'\n yellow:      '0x000000'\n blue:        '0x000000'\n magenta:     '0x000000'\n cyan:        '0x000000'\n white:       '0x000000'\n"
        let
          expected
            = "background:     '0x323232'\n foreground:     '0xffffff'\n \n normal:\n black:       '0x000000'\n red:         '0x010101'\n green:       '0x020202'\n yellow:      '0x030303'\n blue:        '0x040404'\n magenta:     '0x050505'\n cyan:        '0x060606'\n white:       '0x070707'\n \n # Bright colors\n bright:\n black:       '0x080808'\n red:         '0x090909'\n green:       '0x0a0a0a'\n yellow:      '0x0b0b0b'\n blue:        '0x0c0c0c'\n magenta:     '0x0d0d0d'\n cyan:        '0x0e0e0e'\n white:       '0x0f0f0f'\n"
        _configCreator alacritty testTheme config `shouldBe` Right expected
