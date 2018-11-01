{-# LANGUAGE OverloadedStrings #-}

module XTermSpec where

import           TestUtils
import           Apps.XTerm
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
            = "XTerm*foreground: #ffffff\n XTerm*background: #323232\n XTerm*color0: #000000\n XTerm*color8: #080808\n XTerm*color1: #010101\n XTerm*color9: #090909\n XTerm*color2: #020202\n XTerm*color10: #0a0a0a\n XTerm*color3: #030303\n XTerm*color11: #0b0b0b\n XTerm*color4: #040404\n XTerm*color12: #0c0c0c\n XTerm*color5: #050505\n XTerm*color13: #0d0d0d\n XTerm*color6: #060606\n XTerm*color14: #0e0e0e\n XTerm*color7: #070707\n XTerm*color15: #0f0f0f\n"

        _configCreator xTerm testTheme config `shouldBe` Right expected
