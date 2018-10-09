{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Lib
import           Alacritty
import           Test.Hspec
import           Data.Map                      as DM
import           Text.RawString.QQ
import           Text.Parser.Combinators
import           Text.Trifecta
import           Text.PrettyPrint.ANSI.Leijen


prs :: Parser a -> String -> Maybe a
prs p s =
  let r = parseString p mempty s
  in  case r of
        (Success a      ) -> Just a
        (Failure errInfo) -> Nothing

main :: IO ()
main = hspec $ do
  let theme = Theme "foo" $ DM.fromList [("foreground", "#ffffff"),("background", "#bbbbbb"),("color0", "#000000"), ("color1", "#111111"), ("color2", "#222222"), ("color3", "#333333"), ("color4", "#444444"), ("color5", "#555555"), ("color6", "#666666"), ("color7", "#777777"), ("color8", "#888888"), ("color9", "#999999"), ("color10", "#101010"), ("color11", "#111111"), ("color12", "#121212"), ("color13", "#131313"), ("color14", "#141414"), ("color15", "#151515")]
  describe "parseMode" $ do
    it "should parse bright"
      $          prs parseMode "bright: # this is just foo"
      `shouldBe` Just (Mode' Bright)
    it "should parse normal"
      $          prs parseMode "normal: # this is just foo"
      `shouldBe` Just (Mode' Normal)
  describe "parseColor" $ do
    it "should parse color names"
      $          prs parseColor "      black: '0x000000'"
      `shouldBe` Just (Color' "black")
    it "should skip commented lines"
      $          prs parseColor "#  black: '0x000000'"
      `shouldBe` Nothing
  describe "parseColorLine" $ do
    it "should parse trailing and leading content of a color line"
      $          prs parseColorLine "   black: '0x000000' # foo"
      `shouldBe` Just ("   black: ", " # foo")
    it "should skip commented lines"
      $          prs parseColorLine "#   black: '0x000000' # foo"
      `shouldBe` Nothing
  describe "getThemeColor" $ do
    it "should return normal colors" $
      getThemeColor theme "black" Normal `shouldBe` Just "#000000"
    it "should return bright colors" $
      getThemeColor theme "black" Bright `shouldBe` Just "#888888"
  describe "getNewline"
    $          it "should return new line with color replaced"
    $          getNewline "#000000" "    black: '0xFFFFFF' # foo"
    `shouldBe` "    black: '0x000000' # foo"
  describe "getNewlineFromColorName" $
    it "should return newline with color replaced" $
      getNewlineFromColorName theme "black" Normal "black: '0xFFFFFF'" `shouldBe` "black: '0x000000'"
  describe "configCreator'"
    $ it "should return a new config based on the theme"
    $ do
      let config = [r|
background:     '0x000000'
foreground:     '0x000000'

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
      let expected = [r|
background:     '0xbbbbbb'
foreground:     '0xffffff'

normal:
    black:       '0x000000'
    red:         '0x111111'
    green:       '0x222222'
    yellow:      '0x333333'
    blue:        '0x444444'
    magenta:     '0x555555'
    cyan:        '0x666666'
    white:       '0x777777'

# Bright colors
bright:
    black:       '0x888888'
    red:         '0x999999'
    green:       '0x101010'
    yellow:      '0x111111'
    blue:        '0x121212'
    magenta:     '0x131313'
    cyan:        '0x141414'
    white:       '0x151515'
|]
      configCreator' theme config `shouldBe` expected
