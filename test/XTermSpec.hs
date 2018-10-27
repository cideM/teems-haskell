{-# LANGUAGE OverloadedStrings #-}

module XTermSpec where

import           TestUtils
import           XTerm
import           Lib
import           Test.Hspec

spec :: Spec
spec =
  describe "configCreator'"
    $ it "should replace colors with those from the theme"
    $ do
        let
          config
            = "XTerm*foreground: #c5c8c6\n XTerm*background: #1d1f21\n XTerm*color0: #1d1f21\n XTerm*color8: #969896\n XTerm*color1: #cc6666\n XTerm*color9: #cc6666\n XTerm*color2: #b5bd68\n XTerm*color10: #b5bd68\n XTerm*color3: #f0c674\n XTerm*color11: #f0c674\n XTerm*color4: #81a2be\n XTerm*color12: #81a2be\n XTerm*color5: #b294bb\n XTerm*color13: #b294bb\n XTerm*color6: #8abeb7\n XTerm*color14: #8abeb7\n XTerm*color7: #c5c8c6\n XTerm*color15: #ffffff\n"
        let
          expected
            = "XTerm*foreground: #ffffff\n XTerm*background: #bbbbbb\n XTerm*color0: #000000\n XTerm*color8: #888888\n XTerm*color1: #111111\n XTerm*color9: #999999\n XTerm*color2: #222222\n XTerm*color10: #101010\n XTerm*color3: #333333\n XTerm*color11: #111111\n XTerm*color4: #444444\n XTerm*color12: #121212\n XTerm*color5: #555555\n XTerm*color13: #131313\n XTerm*color6: #666666\n XTerm*color14: #141414\n XTerm*color7: #777777\n XTerm*color15: #151515\n"

        configCreator xTerm testTheme config `shouldBe` expected
