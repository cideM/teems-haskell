{-# LANGUAGE OverloadedStrings #-}

module ColorsSpec where

import           Colors
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
  describe "hexP" $ it "should parse valid hex colors with 6 letters" $ do
    prs hexP "#ffffff" `shouldBe` Just (HexColor ("ff", "ff", "ff"))
  describe "hexColorToRGBA" $ it "should convert hex color to rgba" $ do
    hexColorToRGBA (HexColor ("ff", "ff", "ff"))
      `shouldBe` RGBA (255, 255, 255, 1.0)
  describe "rgbaToHexColor" $ it "should convert rgba color to hex color" $ do
    rgbaToHexColor (RGBA (255, 255, 255, 1.0))
      `shouldBe` HexColor ("ff", "ff", "ff")
    rgbaToHexColor (RGBA (10, 10, 10, 1.0))
      `shouldBe` HexColor ("0a", "0a", "0a")
