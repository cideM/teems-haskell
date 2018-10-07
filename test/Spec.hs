{-# LANGUAGE OverloadedStrings #-}

import           Alacritty
import           Test.Hspec
import           Text.Parser.Combinators
import           Text.Trifecta
import           Text.PrettyPrint.ANSI.Leijen


prs :: Parser a -> String -> Either String a
prs p s =
  let r = parseString p mempty s
  in  case r of
        (Success a) -> Right a
        (Failure errInfo) -> Left . show . plain $ _errDoc errInfo

main :: IO ()
main = hspec $ do
  describe "parseMode" $ do
    it "should parse bright"
      $          prs parseMode "bright: # this is just foo"
      `shouldBe` Right (Mode' Bright)
    it "should parse normal"
      $          prs parseMode "nor2mal: # this is just foo"
      `shouldBe` Right (Mode' Normal)
