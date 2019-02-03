{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module XTermSpec where

import           Apps.Internal.XTerm
import           Data.String.QQ
import           Test.Hspec
import           TestUtils
import           Types.Internal.Misc

spec :: Spec
spec =
  describe "configCreator'" $
    it "should replace colors with those from the theme" $ do
      let config =
            [s|
            XTerm*foreground: #c5c8c6
            XTerm*background: #1d1f21
            XTerm*color0: #1d1f21
            XTerm*color8: #969896
            XTerm*color1: #cc6666
            XTerm*color9: #cc6666
            XTerm*color2: #b5bd68
            XTerm*color10: #b5bd68
            XTerm*color3: #f0c674
            XTerm*color11: #f0c674
            XTerm*color4: #81a2be
            XTerm*color12: #81a2be
            XTerm*color5: #b294bb
            XTerm*color13: #b294bb
            XTerm*color6: #8abeb7
            XTerm*color14: #8abeb7
            XTerm*color7: #c5c8c6
            XTerm*color15: #ffffff
|]
      let expected =
            [s|
            XTerm*foreground: #ffffff
            XTerm*background: #323232
            XTerm*color0: #000000
            XTerm*color8: #080808
            XTerm*color1: #010101
            XTerm*color9: #090909
            XTerm*color2: #020202
            XTerm*color10: #0a0a0a
            XTerm*color3: #030303
            XTerm*color11: #0b0b0b
            XTerm*color4: #040404
            XTerm*color12: #0c0c0c
            XTerm*color5: #050505
            XTerm*color13: #0d0d0d
            XTerm*color6: #060606
            XTerm*color14: #0e0e0e
            XTerm*color7: #070707
            XTerm*color15: #0f0f0f
|]
      _configCreator xTerm testTheme config `shouldBe` Right expected
