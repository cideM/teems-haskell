{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import           Test.Hspec
import           Data.Aeson
import           Types
import           Data.Map.Strict               as Map

spec :: Spec
spec = do
  describe "Theme type for aeson"
    $ it "should parse themes"
    $ let theme =
            "{ \"name\": \"foo\", \"colors\": { \"color0\": [24, 24, 24, 1] } }"
      in  eitherDecode theme `shouldBe` Right
            Theme
              { name   = "foo"
              , colors = Map.fromList [("color0", RGBA (24, 24, 24, 1.0))]
              }
  describe "rgbaToHexColor" $ it "should convert rgba color to hex color" $ do
    rgbaToHexColor (RGBA (255, 255, 255, 1.0))
      `shouldBe` HexColor ("ff", "ff", "ff")
    rgbaToHexColor (RGBA (10, 10, 10, 1.0))
      `shouldBe` HexColor ("0a", "0a", "0a")
