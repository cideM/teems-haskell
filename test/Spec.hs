{-# LANGUAGE OverloadedStrings #-}

import           Alacritty
import           Test.Hspec
import           Text.Parser.Combinators
import           Text.Trifecta

prs p = parseString p mempty

maybeSuccess :: Result a -> Either String a
maybeSuccess (Success a) = Right a
maybeSuccess (Failure err) = Left . show $ _errDoc err

main :: IO ()
main = hspec $ do
  describe "parseMode" $ do
    it "should parse bright"
      $ let m = maybeSuccess $ prs parseMode "bright: # this is just foo"
        in  m `shouldBe` Right (Mode' Bright)
    it "should parse normal"
      $ let m = maybeSuccess $ prs parseMode "normal: # this is just foo"
        in  m `shouldBe` Right (Mode' Normal)
